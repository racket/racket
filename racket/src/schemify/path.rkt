#lang racket/base
(require racket/private/relative-path
         racket/private/truncate-path
         "match.rkt"
         "path-for-srcloc.rkt")

(provide extract-paths-from-schemified-linklet
         make-path->compiled-path
         compiled-path->path)

;; Recognize lifted paths in a schemified linklet, and
;; return the list of path values. If `convert?`, then
;; change the schemified linklet to expect the paths
;; as arguments.
(define (extract-paths-from-schemified-linklet linklet-e convert?)
  (match linklet-e
    [`(lambda . ,_)
     ;; No constants, so no paths:
     (values '() linklet-e)]
    [`(let* ,bindings ,body)
     (define (path-binding? b)
       (define rhs (cadr b))
       (or (path? rhs) (path-for-srcloc? rhs)))
     (define any-path?
       (for/or ([b (in-list bindings)])
         (path-binding? b)))
     (cond
       [any-path?
        (define paths (for/list ([b (in-list bindings)]
                                 #:when (path-binding? b))
                        (cadr b)))
        (cond
          [convert?
           (define path-ids (for/list ([b (in-list bindings)]
                                       #:when (path-binding? b))
                              (car b)))
           (define other-bindings (for/list ([b (in-list bindings)]
                                             #:unless (path-binding? b))
                                    b))
           (values paths
                   `(lambda ,path-ids
                      (let* ,other-bindings ,body)))]
          [else
           (values paths linklet-e)])]
       [else
        (values '() linklet-e)])]))

(define (make-path->compiled-path who)
  (define path->relative-path-elements (make-path->relative-path-elements #:who who))
  (lambda (orig-p)
    (define p (if (path-for-srcloc? orig-p)
                  (path-for-srcloc-path orig-p)
                  orig-p))
    (or (path->relative-path-elements p)
        (cond
          [(path-for-srcloc? orig-p)
           ;; Can't make relative, so create a string that keeps up
           ;; to two path elements
           (truncate-path p)]
          [else (path->bytes p)]))))

(define (compiled-path->path e)
  (cond
    [(bytes? e) (bytes->path e)]
    [(string? e) e] ; was `path-for-srcloc` on write
    [else (relative-path-elements->path e)]))
