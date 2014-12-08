#lang racket/base
(require racket/list
         racket/set
         racket/file
         setup/getinfo
         "get-info.rkt"
         "dep.rkt"
         "metadata.rkt"
         "stage.rkt")

(provide extract-pkg-dependencies
         get-pkg-content)

(define (extract-pkg-dependencies get-info
                                  #:build-deps? [build-deps? #t]
                                  #:filter? [filter? #f]
                                  #:versions? [versions? #f])
  (define v (if get-info
                (get-info 'deps (lambda () empty))
                empty))
  ((check-dependencies 'deps) v)
  (define v2 (if (and get-info build-deps?)
                 (get-info 'build-deps (lambda () empty))
                 empty))
  ((check-dependencies 'build-deps) v2)
  (define all-v (append v v2))
  (if filter?
      (for/list ([dep (in-list all-v)]
                 #:when (dependency-this-platform? dep))
        (define name
          (if (pair? dep)
              (car dep)
              dep))
        (if versions?
            (list name (dependency->version dep))
            name))
      all-v))

(define (get-pkg-content desc 
                         #:namespace [metadata-ns (make-metadata-namespace)]
                         #:extract-info [extract-info extract-pkg-dependencies]
                         #:use-cache? [use-cache? #f]
                         #:quiet? [quiet? #t])
  (define-values (pkg-name dir cksum clean? module-paths) 
    (pkg-stage desc
               #:in-place? #t
               #:namespace metadata-ns
               #:use-cache? use-cache?
               #:quiet? quiet?))
  (define get-info (get-info/full dir #:namespace metadata-ns))
  (begin0
   (values cksum
           (set->list module-paths)
           (extract-info get-info))
   (when clean?
     (delete-directory/files dir))))
