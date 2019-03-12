#lang racket/base
(require racket/private/relative-path
         racket/private/truncate-path
         racket/fasl
         "match.rkt"
         "path-for-srcloc.rkt"
         "to-fasl.rkt")

(provide extract-paths-from-schemified-linklet
         make-path->compiled-path
         compiled-path->path
         force-unfasl)

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
       (or (path? rhs) (path-for-srcloc? rhs) (to-fasl? rhs)))
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
    (cond
      [(to-fasl? orig-p)
       (box (s-exp->fasl (force-unfasl orig-p)))]
      [else
       (define p (if (path-for-srcloc? orig-p)
                     (path-for-srcloc-path orig-p)
                     orig-p))
       (cond
         [(path? p)
          (or (path->relative-path-elements p)
              (cond
                [(path-for-srcloc? orig-p)
                 ;; Can't make relative, so create a string that keeps up
                 ;; to two path elements
                 (truncate-path p)]
                [else (path->bytes p)]))]
         [(or (string? p) (bytes? p) (symbol? p) (not p))
          ;; Allowed in compiled form
          p]
         [else
          (error 'write
                 "cannot marshal value that is embedded in compiled code: ~V"
                 p)])])))

(define (compiled-path->path e)
  (cond
    [(box? e) (to-fasl (box (unbox e))
                       (or (current-load-relative-directory)
                           (current-directory)))]
    [(bytes? e) (bytes->path e)]
    [(string? e) e] ; was `path-for-srcloc` on write
    [else (relative-path-elements->path e)]))

(define (force-unfasl tf)
  (define vb (to-fasl-vb tf))
  (define v (unbox vb))
  (cond
    [(bytes? v)
     (define v2 (parameterize ([current-load-relative-directory (to-fasl-wrt tf)])
                  (fasl->s-exp v #:datum-intern? #t)))
     (box-cas! vb v v2)
     (set-to-fasl-wrt! tf #f)
     (unbox vb)]
    [else
     ;; already forced (or never fasled)
     v]))
