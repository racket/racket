#lang racket/base

(require pkg/lib         
         (only-in planet/config [CACHE-DIR find-planet-dir])
         "../dirs.rkt"
         "../path-to-relative.rkt")

(provide path->relative-string/setup/pkg)

(define path->relative-string/setup/pkg
  (let ()
    (define current-cache (make-parameter #f))
    (define p->r
      (make-path->relative-string
       (list (cons find-collects-dir      "<collects>/")
             (cons find-user-collects-dir "<user>/")
             (cons find-planet-dir        "<planet>/"))
       (lambda (x)
         (define-values (pkg sub) (path->pkg+subpath x #:cache (current-cache)))
         (cond
          [pkg
           (string-append "<pkgs>" "/" pkg "/" (if (eq? sub 'same) "" (path->string sub)))]
          [(path? x) (path->string x)]
          [else x]))))
    (lambda (x #:cache [cache #f])
      (parameterize ([current-cache cache])
        (p->r x)))))
