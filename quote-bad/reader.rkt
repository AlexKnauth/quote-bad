#lang racket/base

(provide wrap-reader)

(require hygienic-quote/private/make-quote-proc
         (for-meta -10 (only-in quote-bad/quote-bad quote))
         (for-meta -9 (only-in quote-bad/quote-bad quote))
         (for-meta -8 (only-in quote-bad/quote-bad quote))
         (for-meta -7 (only-in quote-bad/quote-bad quote))
         (for-meta -6 (only-in quote-bad/quote-bad quote))
         (for-meta -5 (only-in quote-bad/quote-bad quote))
         (for-meta -4 (only-in quote-bad/quote-bad quote))
         (for-meta -3 (only-in quote-bad/quote-bad quote))
         (for-meta -2 (only-in quote-bad/quote-bad quote))
         (for-meta -1 (only-in quote-bad/quote-bad quote))
         (for-meta 0 (only-in quote-bad/quote-bad quote))
         (for-meta 1 (only-in quote-bad/quote-bad quote))
         (for-meta 2 (only-in quote-bad/quote-bad quote))
         (for-meta 3 (only-in quote-bad/quote-bad quote))
         (for-meta 4 (only-in quote-bad/quote-bad quote))
         (for-meta 5 (only-in quote-bad/quote-bad quote))
         (for-meta 6 (only-in quote-bad/quote-bad quote))
         (for-meta 7 (only-in quote-bad/quote-bad quote))
         (for-meta 8 (only-in quote-bad/quote-bad quote))
         (for-meta 9 (only-in quote-bad/quote-bad quote))
         (for-meta 10 (only-in quote-bad/quote-bad quote))
         )

;; wrap-reader : [A ... -> Any] -> [A ... -> Any]
(define (wrap-reader p)
  (extend-reader p make-quote-bad-readtable))

;; make-quote-bad-readtable : Readtable [Syntax -> Syntax] -> Readtable
(define (make-quote-bad-readtable orig-rt outer-scope)
  (make-readtable orig-rt
    #\' 'terminating-macro (make-quote-proc #'quote outer-scope)
    ))

