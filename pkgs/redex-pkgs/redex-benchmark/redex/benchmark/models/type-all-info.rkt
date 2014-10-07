#lang racket/base

(require racket/runtime-path
         racket/list
         racket/path)

(provide all-mods)

(define-runtime-path here ".")

(define (all-info-files)
  (for/list ([f (in-directory here)]
             #:when (and (file-exists? f)
                         (regexp-match #rx"^.*typed-info\\.rkt$"
                                       (path->string f)))
             #:unless (regexp-match #rx".*all-info.*"
                                    (path->string f)))
    (path->string 
     (find-relative-path (simplify-path (current-directory))
                         (simplify-path f)))))

(define (all-mods)
  (append-map (λ (f)
                ((dynamic-require f 'all-mods)))
              (all-info-files)))
