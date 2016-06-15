#lang racket/base

(require syntax/module-reader)

(provide (rename-out [-read read]
                     [-read-syntax read-syntax]
                     [-get-info get-info]))

(define-values (-read -read-syntax -get-info)
  (make-meta-reader
   'constructor-style-print
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
   (lambda (orig-read)
     orig-read)
   (lambda (orig-read-syntax)
     (define (read-syntax . args)
       (define stx (apply orig-read-syntax args))
       (define old-prop (syntax-property stx 'module-language))
       (define new-prop `#[constructor-style-print/lang/language-info
                           get-language-info
                           ,old-prop])
       (syntax-property stx 'module-language new-prop))
     read-syntax)
   (lambda (proc) proc)))
