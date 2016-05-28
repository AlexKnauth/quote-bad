#lang racket/base

(provide quote)

(require (only-in racket/base [quote rkt:quote])
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/port
                     racket/pretty
                     syntax/parse
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
  ;; translate-quoted-s-expr : Stx -> S-Expr
  (define (translate-quoted-s-expr stuff)
    (cond
      [(syntax? stuff) (translate-quoted-s-expr (syntax-e stuff))]
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

