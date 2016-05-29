#lang racket/base

(provide quote #%datum)

(require (only-in racket/base [quote rkt:quote] [#%datum rkt:#%datum])
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/port
                     racket/pretty
                     syntax/parse
                     ))
(module+ test
  (require racket/match
           rackunit
           syntax/macro-testing
           ))

(begin-for-syntax
  ;; raise-quote-bad-error : Syntax Syntax -> Nothing
  ;; Raises a syntax error explaining what to use instead of quote.
  (define (raise-quote-bad-error stx stuff)
    ;; translated-s-expr : S-Expr
    (define translated-s-expr
      (translate-quoted-s-expr stuff))
    ;; translated-pretty : String
    (define translated-pretty
      (pretty-write-string translated-s-expr))
    ;; message : String
    (define message
      (format "Don't use quote for this. Instead you can use\n~a" translated-pretty))
    (raise-syntax-error #f message stx))
  ;; raise-self-quote-bad-error : Syntax Syntax -> Nothing
  ;; Raises a syntax error explaining what to use instead of a self-quoting literal.
  (define (raise-self-quote-bad-error stx stuff)
    ;; translated-s-expr : S-Expr
    (define translated-s-expr
      (translate-quoted-s-expr stuff))
    ;; translated-pretty : String
    (define translated-pretty
      (pretty-write-string translated-s-expr))
    ;; message : String
    (define message
      (format (string-append
               "Don't use self-quoting compound literals that implicitly quote sub-expressions.\n"
               "Instead you can use\n"
               "~a")
              translated-pretty))
    (raise-syntax-error #f message stx))
  ;; translate-quoted-s-expr : Stx -> S-Expr
  (define (translate-quoted-s-expr stuff)
    (cond
      [(syntax? stuff) (translate-quoted-s-expr (or (syntax->list stuff) (syntax-e stuff)))]
      [(symbol? stuff) (list 'quote stuff)] ; the one place this recommends quote
      [(atomic-literal-data? stuff) stuff]
      [(list? stuff) (list* 'list (map translate-quoted-s-expr stuff))]
      [(cons? stuff) (translate-quoted-cons-s-expr stuff)]
      [(vector? stuff) (list* 'vector-immutable (map translate-quoted-s-expr (vector->list stuff)))]
      [(hash? stuff) (translate-quoted-hash-s-expr stuff)]
      [(box? stuff) (list 'box-immutable (translate-quoted-s-expr (unbox stuff)))]
      [else '....]))
  ;; atomic-literal-data? : Any -> Boolean
  (define (atomic-literal-data? stuff)
    (or (boolean? stuff)
        (number? stuff)
        (string? stuff)
        (bytes? stuff)
        (keyword? stuff)
        (regexp? stuff)
        (byte-regexp? stuff)
        ))
  ;; translate-quoted-cons-s-expr : (Cons Stx Stx) -> S-Expr
  (define (translate-quoted-cons-s-expr stuff)
    (match stuff
      [(cons a (and b (not (? cons?))))
       (list 'cons (translate-quoted-s-expr a) (translate-quoted-s-expr b))]
      [(list-rest as ... (and b (not (? cons?))))
       (append (list 'list*)
               (map translate-quoted-s-expr as)
               (list (translate-quoted-s-expr b)))]))
  ;; translate-quoted-hash-s-expr : (Hashof Stx Stx) -> S-Expr
  (define (translate-quoted-hash-s-expr stuff)
    (define hash-proc-args
      (append* (for/list ([(k v) (in-hash stuff)])
                 (list (translate-quoted-s-expr k)
                       (translate-quoted-s-expr v)))))
    (cond
      [(hash-equal? stuff)
       (list* 'hash hash-proc-args)]
      [(hash-eqv? stuff)
       (list* 'hasheqv hash-proc-args)]
      [(hash-eq? stuff)
       (list* 'hasheq hash-proc-args)]
      [else '....]))
  ;; pretty-write-string : S-Expr -> String
  (define (pretty-write-string s-expr)
    (call-with-output-string
     (λ (out)
       (pretty-write s-expr out))))
  )

(define-syntax quote
  (lambda (stx)
    (syntax-parse stx
      [(quote id:id)
       (syntax/loc stx (rkt:quote id))]
      [(quote simple)
       #:when (atomic-literal-data? (syntax-e #'simple))
       (syntax/loc stx (rkt:quote simple))]
      [(quote stuff)
       (raise-quote-bad-error stx #'stuff)])))

(define-syntax #%datum
  (lambda (stx)
    (syntax-parse stx
      [(#%datum . id:id) ; for some reason (#%datum . id) expands (quote id)
       (syntax/loc stx (rkt:#%datum . id))]
      [(#%datum . simple)
       #:when (atomic-literal-data? (syntax-e #'simple))
       (syntax/loc stx (rkt:#%datum . simple))]
      [(#%datum . stuff)
       (raise-self-quote-bad-error stx #'stuff)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-syntax check-error
    (lambda (stx)
      (syntax-parse stx
        [(check-stxerr exn-predicate expr)
         (syntax/loc stx
           (check-exn exn-predicate
                      (λ () (convert-compile-time-error expr))))])))

  (define-match-expander *q*
    (lambda (stx)
      (syntax-parse stx
        [(*q* s-expr)
         #'(== (rkt:quote s-expr))])))

  (define (expected-msg* msg s-expr-matches?)
    (define n (string-length msg))
    (define (pred e)
      (and (exn:fail? e)
           (< n (string-length (exn-message e)))
           (string=? (substring (exn-message e) 0 n) msg)
           (s-expr-matches? (read (open-input-string (substring (exn-message e) n))))))
    pred)
  (define-syntax-rule (expected-msg msg s-expr-pat)
    (expected-msg* msg (λ (v) (match v [s-expr-pat #true] [_ #false]))))

  (test-case "okay uses of quote, with symbols and simple atomic literal data"
    (check-equal? 'abc (string->symbol "abc"))
    (check-equal? 'def (string->symbol "def"))
    (check-equal? '#true #true)
    (check-equal? '#false #false)
    (check-equal? '2 2)
    (check-equal? '4.5 4.5)
    (check-equal? '"abc" "abc")
    (check-equal? '"def" "def")
    (check-equal? '#"abc" #"abc")
    (check-equal? '#"def" #"def")
    (check-equal? '#:abc (string->keyword "abc"))
    (check-equal? '#:def (string->keyword "def"))
    )

  (test-case "bad uses of quote, with lists, vectors, and other compound literal data"
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (list)))
                 '())
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (list 'a 'b 'c (list 'd) (list 'e (list 'f)) (list (list 'g)))))
                 '(a b c (d) (e (f)) ((g))))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (cons 'a 'b)))
                 '(a . b))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f))))
                 '([a . b] [c . d] [e . f]))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (list* 'a 'b 'c)))
                 '(a b . c))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (list 'a 'b 'c)))
                 '(a b . (c)))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (vector-immutable 'a 'b 'c (list 'd)
                                       (vector-immutable 'e (list 'f))
                                       (list (list 'g)))))
                 '#(a b c (d) #(e (f)) ((g))))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (or (*q* (hash 'a 'b 'c 'd 'e 'f))
                      (*q* (hash 'a 'b 'e 'f 'c 'd))
                      (*q* (hash 'c 'd 'a 'b 'e 'f))
                      (*q* (hash 'c 'd 'e 'f 'a 'b))
                      (*q* (hash 'e 'f 'a 'b 'c 'd))
                      (*q* (hash 'e 'f 'c 'd 'a 'b))))
                 '#hash([a . b] [c . d] [e . f]))
    (check-error (expected-msg
                  "quote: Don't use quote for this. Instead you can use"
                  (*q* (box-immutable 'a)))
                 '#&a)
    )
  )
