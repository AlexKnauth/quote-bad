#lang racket/base

(provide wrap-reader)

(require hygienic-quote/private/make-quote-proc
         syntax/parse
         "translate-quoted.rkt"
         "quote-bad-error.rkt"
         (for-meta -10 racket/base)
         (for-meta -9 racket/base)
         (for-meta -8 racket/base)
         (for-meta -7 racket/base)
         (for-meta -6 racket/base)
         (for-meta -5 racket/base)
         (for-meta -4 racket/base)
         (for-meta -3 racket/base)
         (for-meta -2 racket/base)
         (for-meta -1 racket/base)
         (for-meta 0 racket/base)
         (for-meta 1 racket/base)
         (for-meta 2 racket/base)
         (for-meta 3 racket/base)
         (for-meta 4 racket/base)
         (for-meta 5 racket/base)
         (for-meta 6 racket/base)
         (for-meta 7 racket/base)
         (for-meta 8 racket/base)
         (for-meta 9 racket/base)
         (for-meta 10 racket/base)
         )

;; wrap-reader : [A ... -> Any] -> [A ... -> Any]
(define (wrap-reader p)
  (extend-reader p make-quote-bad-readtable))

;; make-quote-bad-readtable : Readtable [Syntax -> Syntax] -> Readtable
(define (make-quote-bad-readtable orig-rt outer-scope)
  (make-readtable orig-rt
    #\' 'terminating-macro (wrap-quote-proc (make-quote-proc #'quote outer-scope))
    ))

;; wrap-quote-proc : Readtable-Proc -> Readtable-Proc
(define ((wrap-quote-proc quote-proc) char in src ln col pos)
  (define stx (quote-proc char in src ln col pos))
  (syntax-parse stx
    [(quote id:id) stx]
    [(quote kw:keyword) stx]
    [(quote simple) #:when (atomic-literal-data? (syntax-e #'simple)) stx]
    [(quote stuff) (raise-quote-bad-error stx #'stuff)]))

