#lang racket/base
(require (for-syntax racket/base file/gunzip net/base64))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ x ...)
     (andmap (lambda (x) (or (identifier? x) (integer? (syntax-e x))))
             (syntax->list #'(x ...)))
     (let* ([data  (format "~a" (syntax->datum #'(x ...)))]
            [data  (substring data 1 (sub1 (string-length data)))]
            [data  (string->bytes/utf-8 data)]
            [in    (open-input-bytes (base64-decode data))]
            [out   (open-output-string)]
            [out   (begin (inflate in out) (get-output-string out))]
            [exprs (read (open-input-string (string-append "(" out ")")))]
            [exprs (datum->syntax stx exprs stx)])
       #`(#%module-begin #,@exprs))]))
