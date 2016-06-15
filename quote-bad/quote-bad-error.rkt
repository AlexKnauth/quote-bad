#lang racket/base

(provide raise-quote-bad-error
         raise-self-quote-bad-error
         )

(require racket/port
         racket/pretty
         "translate-quoted.rkt"
         )

;; raise-quote-bad-error : Syntax Syntax -> Nothing
;; Raises a syntax error explaining what to use instead of quote.
(define (raise-quote-bad-error stx stuff)
  ;; translated-s-expr : S-Expr
  (define translated-s-expr
    (translate-quoted stuff))
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
    (translate-quoted stuff))
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

;; pretty-write-string : S-Expr -> String
(define (pretty-write-string s-expr)
  (call-with-output-string
   (λ (out)
     (pretty-write s-expr out))))

