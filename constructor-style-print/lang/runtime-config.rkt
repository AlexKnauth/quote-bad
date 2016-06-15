#lang racket/base

(provide configure)

(require mzlib/pconvert-prop
         "../constructor-style-print.rkt"
         )

(define (configure data)
  (global-port-print-handler
   (make-constructor-style-port-print-handler (global-port-print-handler)))
  (current-print
   (make-constructor-style-printer (current-print))))

;; make-constructor-style-print-handler :
;; (Any Output-Port [(U 0 1)] -> Void) -> (Any Output-Port [(U 0 1)] -> Void)
(define (make-constructor-style-port-print-handler orig-port-print-handler)
  ;; constructor-style-port-print-handler : Any Output-Port [(U 0 1)] -> Void
  (define (constructor-style-port-print-handler v out [qdepth 0])
    (parameterize ([global-port-print-handler orig-port-print-handler])
      (constructor-style-print v out qdepth)))
  constructor-style-port-print-handler)

;; make-constructor-style-printer : (Any -> Void) -> (Any -> Void)
(define (make-constructor-style-printer orig-printer)
  ;; constructor-style-printer : Any -> Void
  (define (constructor-style-printer v)
    (when (not (void? v))
      (orig-printer (written (print-convert/constructor-style v)))))
  constructor-style-printer)

;; A struct for wrapping values that are printed as an s-expression or
;; other value is written
(struct written (v)
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (write (written-v this) out))]
  #:property prop:print-converter
  (lambda (this recur)
    (written-v this)))

