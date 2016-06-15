#lang racket/base

(provide constructor-style-print
         constructor-style-println
         print-convert/constructor-style
         )

(require mzlib/pconvert
         racket/pretty
         )
(module+ test
  (require racket/port rackunit))

;; print-convert/constructor-style : Any -> Any
(define (print-convert/constructor-style v)
  (parameterize ([constructor-style-printing #true]
                 [booleans-as-true/false #false])
    (print-convert v)))

;; constructor-style-print : Any [Output-Port] [(U 0 1)] -> Void
;; Prints v using constructor-style printing (without pretty-printing).
(define (constructor-style-print v [out (current-output-port)] [qdepth 0])
  (parameterize ([print-reader-abbreviations #true])
    (write (print-convert/constructor-style v) out)))

;; constructor-style-println : Any [Output-Port] [(U 0 1)] -> Void
;; Prints v followed by a newline, using constructor-style pretty printing.
(define (constructor-style-println v [out (current-output-port)] [qdepth 0])
  (parameterize ([print-reader-abbreviations #true]
                 [pretty-print-abbreviate-read-macros #true])
    (pretty-write (print-convert/constructor-style v) out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "constructor-style-print"
    (define (str v)
      (call-with-output-string
       (λ (out) (constructor-style-print v out))))
    (define (strln v)
      (call-with-output-string
       (λ (out) (constructor-style-println v out))))
    (define (s v)
      (print-convert/constructor-style v))
    
    (check-equal? (str (list 1 2 3)) "(list 1 2 3)")
    (check-equal? (strln (list 1 2 3)) "(list 1 2 3)\n")
    (check-equal? (str (vector-immutable 1 2 3)) "(vector-immutable 1 2 3)")
    (check-equal? (strln (vector-immutable 1 2 3)) "(vector-immutable 1 2 3)\n")
    (check-equal? (str (list 'a 'b 'c)) "(list 'a 'b 'c)")
    (check-equal? (strln (list 'a 'b 'c)) "(list 'a 'b 'c)\n")

    (check-equal? (s (list 1 2 3)) '(list 1 2 3))
    (check-equal? (s (vector-immutable 1 2 3)) '(vector-immutable 1 2 3))
    (check-equal? (s (list 'a 'b 'c)) '(list 'a 'b 'c))

    (test-case "simple atomic data"
      (check-equal? (s 'abc) ''abc)
      (check-equal? (s #true) #true)
      (check-equal? (s #false) #false)
      (check-equal? (s 2) 2)
      (check-equal? (s 4.5) 4.5)
      (check-equal? (s "abc") "abc")
      (check-equal? (s '#:abc) ''#:abc)
      )
    (test-case "constructed data"
      (check-equal? (s (list)) 'empty)
      (check-equal? (s (list 'a 'b 'c)) '(list 'a 'b 'c))
      (check-equal? (s (list '#:abc)) '(list '#:abc))
      (check-equal? (s (cons 'a 'b)) '(cons 'a 'b))
      (check-equal? (s (list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f)))
                    '(list (cons 'a 'b) (cons 'c 'd) (cons 'e 'f)))
      (check-equal? (s (list* 'a 'b 'c)) '(cons 'a (cons 'b 'c)))
      (check-equal? (s (vector-immutable 'a 'b 'c)) '(vector-immutable 'a 'b 'c))
      (check-match (s (hash 'a 'b 'c 'd 'e 'f))
                   `(make-immutable-hash
                     (list
                      ,@(list-no-order '(cons 'a 'b) '(cons 'c 'd) '(cons 'e 'f)))))
      (check-equal? (s (box-immutable 'a)) '(box-immutable 'a))
      (check-equal? (s (make-prefab-struct 'hello 1 2 3)) '(make-prefab-struct 'hello 1 2 3))
      )
    )
  )
