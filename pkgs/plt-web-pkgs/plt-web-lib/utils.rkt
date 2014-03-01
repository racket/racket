#lang at-exp racket/base

(require scribble/html (for-syntax racket/base))

(provide web-path)
(define (web-path . xs)
  (string-join xs "/"))

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

(provide basename)
(define (basename path)
  (define-values [base file dir?] (split-path path))
  (path->string file))

;; simple file resources
(define ((make-path-resourcer file-op)
         source [target (basename source)] #:dir [dir #f])
  (resource (if (eq? void file-op) (void)
              (if dir (web-path dir target) target))
            (λ (file) (file-op source file))))

(provide copyfile-resource symlink-resource)
(define copyfile-resource (make-path-resourcer copy-file))
(define symlink-resource  (make-path-resourcer make-file-or-directory-link))
