#lang quote-bad/constructor-style-print racket/base

;; Because of how raco test works, this file's test module isn't run
;; as if this file were the main file, so this explicit runtime-config
;; setup is necessary.
(require "../constructor-style-print/lang/runtime-config.rkt")
(configure #f)

(module+ test
  (require rackunit racket/port)
  (define (str v)
    (call-with-output-string
     (λ (out) (print v out))))
  
  (check-equal? (str (list 1 2 3)) "(list 1 2 3)")
  (check-equal? (str (vector-immutable 1 2 3)) "(vector-immutable 1 2 3)")
  (check-equal? (str (list 'a 'b 'c)) "(list 'a 'b 'c)")
  )
