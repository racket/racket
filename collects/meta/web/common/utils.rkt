#lang at-exp s-exp meta/web/html

(require racket/string (for-syntax scheme/base))

(provide in-here)
(define-syntax (in-here stx)
  (syntax-case stx ()
    [(_ path paths ...)
     (let ([src (let-values ([(dir name dir?)
                              (split-path (syntax-source stx))])
                  (or dir (raise-syntax-error
                           'in-here "missing source information" stx)))])
       #`(build-path '#,src path paths ...))]))

(provide copyfile-resource)
(define (copyfile-resource source target [referrer values])
  (resource target (lambda (file) (copy-file source file)) referrer))

(provide web-path)
(define (web-path . xs)
  (string-join xs "/"))

(provide ->string)
(define (->string x)
  (cond [(string? x) x]
        [(path?   x) (path->string x)]
        [(bytes?  x) (bytes->string/utf-8 x)]
        [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [else (error '->string "don't know what to do with ~e" x)]))
