#lang at-exp racket/base

(require scribble/html (for-syntax scheme/base))

(provide in-here)
(define-syntax (in-here stx)
  (syntax-case stx ()
    [(_ path paths ...)
     (let ([src (let-values ([(dir name dir?)
                              (split-path (syntax-source stx))])
                  (or dir (raise-syntax-error
                           'in-here "missing source information" stx)))])
       #`(build-path '#,src path paths ...))]))

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

;; resources with a specific referrer; if the referrer is `values',
;; return a plain resource (which behaves the same)
(provide resource/referrer url-of)
(struct referable (referrer resource) #:property prop:procedure 0)
(define (resource/referrer path renderer referrer)
  (define url (resource path renderer))
  (if (eq? referrer values)
    url
    (referable (λ args (apply referrer (url) args)) url)))
(define (url-of referable [absolute? #f])
  (cond [(referable? referable) ((referable-resource referable) absolute?)]
        [(resource? referable)  (referable absolute?)]
        [else (raise-type-error 'url-of "referable" referable)]))

;; simple file resources
(define ((make-path-resourcer file-op) source [target #f] #:dir [dir #f])
  (let ([target (or target (let-values ([(base file dir?) (split-path source)])
                             (path->string file)))])
    (resource (if (eq? void file-op)
                (void) (if dir (web-path dir target) target))
              (λ (file) (file-op source file)))))

(provide copyfile-resource symlink-resource)
(define copyfile-resource (make-path-resourcer copy-file))
(define symlink-resource  (make-path-resourcer make-file-or-directory-link))
