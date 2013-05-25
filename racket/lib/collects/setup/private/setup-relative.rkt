#lang racket/base

(require pkg/lib         
         (only-in planet/config [CACHE-DIR find-planet-dir])
         "../dirs.rkt"
         "../path-to-relative.rkt")

(provide path->relative-string/setup)

(define path->relative-string/setup
  (make-path->relative-string
   (list (cons find-collects-dir      "<collects>/")
         (cons find-user-collects-dir "<user>/")
         (cons find-planet-dir        "<planet>/"))
   (lambda (x)
     (define-values (pkg sub) (path->pkg+subpath x))
     (cond
      [pkg
       (string-append "<pkgs>" "/" pkg "/" (if (eq? sub 'same) "" (path->string sub)))]
      [(path? x) (path->string x)]
      [else x]))))
