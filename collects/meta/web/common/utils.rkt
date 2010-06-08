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

(define ((make-path-resourcer file-op)
         source [target #f] [referrer values] #:dir [dir #f])
  (let ([target (or target (let-values ([(base file dir?) (split-path source)])
                             (path->string file)))])
    (resource (if (eq? void file-op)
                (void) (if dir (web-path dir target) target))
              (lambda (file) (file-op source file)) referrer)))

(provide copyfile-resource symlink-resource)
(define copyfile-resource (make-path-resourcer copy-file))
(define symlink-resource  (make-path-resourcer make-file-or-directory-link))

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
