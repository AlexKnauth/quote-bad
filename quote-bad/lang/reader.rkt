#lang racket/base

(provide (rename-out [quote-bad-read read]
                     [quote-bad-read-syntax read-syntax]
                     [quote-bad-get-info get-info]))

(require syntax/module-reader
         (only-in "../reader.rkt" wrap-reader))

(define-values [quote-bad-read quote-bad-read-syntax quote-bad-get-info]
  (make-meta-reader
   'quote-bad
   "language path"
   (lambda (bstr)
     (let* ([str (bytes->string/latin-1 bstr)]
            [sym (string->symbol str)])
       (and (module-path? sym)
            (vector
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader"))))))
   wrap-reader ; for read
   wrap-reader ; for read-syntax
   (lambda (proc)
     (lambda (key defval)
       (define (fallback) (if proc (proc key defval) defval))
       (fallback)))))
