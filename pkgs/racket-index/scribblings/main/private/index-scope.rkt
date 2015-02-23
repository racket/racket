#lang racket/base
(require setup/dirs
         racket/path)

(provide index-at-user?)

(define (index-at-user?)
  (define p
    (find-relative-path 
     (collection-file-path "index-scope.rkt" "scribblings/main/private")
     (simple-form-path (find-user-pkgs-dir))
     #:more-than-root? #t))
  (and (path? p)
       (let loop ([p p])
         (let-values ([(base name dir?) (split-path p)])
           (and (eq? name 'up)
                (or (and (path? base)
                         (loop base))
                    (eq? base 'relative)))))))
