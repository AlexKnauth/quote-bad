#lang racket/base

(provide configure)

(require mzlib/pconvert-prop
         "../constructor-style-print.rkt"
         )

(define (configure data)
  (define orig-port-print-handler (global-port-print-handler))
  ;; constructor-style-port-print-handler : Any Output-Port [(U 0 1)] -> Void
  ;; The new value for the global-port-print-handler.
  (define (constructor-style-port-print-handler v out [qdepth 0])
    (parameterize ([global-port-print-handler orig-port-print-handler])
      (constructor-style-print v out qdepth)))
  (global-port-print-handler constructor-style-port-print-handler))



;; A struct for wrapping values that are printed as an s-expression or
;; other value is written
(struct written (v)
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (write (written-v this) out))]
  #:property prop:print-converter
  (lambda (this recur)
    (written-v this)))

