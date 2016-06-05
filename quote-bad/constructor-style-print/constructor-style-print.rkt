#lang racket/base

(provide constructor-style-print
         constructor-style-println
         )

(require mzlib/pconvert
         racket/pretty
         )
(module+ test
  (require racket/port rackunit))

;; constructor-style-print : Any [Output-Port] [(U 0 1)] -> Void
;; Prints v using constructor-style printing (without pretty-printing).
(define (constructor-style-print v [out (current-output-port)] [qdepth 0])
  (parameterize ([constructor-style-printing #true]
                 [booleans-as-true/false #false]
                 [print-reader-abbreviations #true])
    (write (print-convert v) out)))

;; constructor-style-println : Any [Output-Port] [(U 0 1)] -> Void
;; Prints v followed by a newline, using constructor-style pretty printing.
(define (constructor-style-println v [out (current-output-port)] [qdepth 0])
  (parameterize ([constructor-style-printing #true]
                 [booleans-as-true/false #false]
                 [print-reader-abbreviations #true]
                 [pretty-print-abbreviate-read-macros #true])
    (pretty-write (print-convert v) out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define (str v)
    (call-with-output-string
     (λ (out) (constructor-style-print v out))))
  (define (strln v)
    (call-with-output-string
     (λ (out) (constructor-style-println v out))))

  (check-equal? (str (list 1 2 3)) "(list 1 2 3)")
  (check-equal? (strln (list 1 2 3)) "(list 1 2 3)\n")
  (check-equal? (str (vector-immutable 1 2 3)) "(vector-immutable 1 2 3)")
  (check-equal? (strln (vector-immutable 1 2 3)) "(vector-immutable 1 2 3)\n")
  (check-equal? (str (list 'a 'b 'c)) "(list 'a 'b 'c)")
  (check-equal? (strln (list 'a 'b 'c)) "(list 'a 'b 'c)\n")
  )
