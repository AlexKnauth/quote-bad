#lang racket/base

(provide quote #%datum)

(require (only-in racket/base [quote rkt:quote] [#%datum rkt:#%datum])
         (for-syntax racket/base
                     syntax/parse
                     "translate-quoted.rkt"
                     "quote-bad-error.rkt"
                     ))
(module+ test
  (require racket/match
           rackunit
           (only-in unstable/macro-testing convert-compile-time-error)
           ))

(define-syntax quote
  (lambda (stx)
    (syntax-parse stx
      [(quote id:id)
       (syntax/loc stx (rkt:quote id))]
      [(quote kw:keyword)
       (syntax/loc stx (rkt:quote kw))]
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
           (match (exn-message e)
             [(regexp (regexp (string-append (regexp-quote msg) "(.*)$"))
                      (list _ s-expr-string))
              (s-expr-matches? (read (open-input-string s-expr-string)))]
             [_ #false])))
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
                  (*q* (list '#:abc)))
                 '(#:abc))
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
