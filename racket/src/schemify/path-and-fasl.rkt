#lang racket/base
(require racket/private/relative-path
         racket/private/truncate-path
         racket/fasl
         "match.rkt"
         "path-for-srcloc.rkt"
         "to-fasl.rkt")

(provide extract-paths-and-fasls-from-schemified-linklet
         make-path->compiled-path
         compiled-path->path
         force-unfasl)

;; Recognize lifted paths and `to-fasl`s in a schemified linklet, and
;; return the list of path and `to-fasl` values. If `convert?`, then
;; change the schemified linklet to expect the paths as arguments.
;;
;; In addition to paths, this extraction deals with values that have
;; been packages as `to-fasl`, either because they are large values
;; that are best handled in fasl form, because they are not
;; serializable (and we want to delay complaining in case no
;; serialization is needed), or because they are uninterned symbols
;; that need to be exposed to the Scheme-level `fasl` for a full
;; linklet.

(define (extract-paths-and-fasls-from-schemified-linklet linklet-e convert?)
  (match linklet-e
    [`(lambda . ,_)
     ;; No constants, so no paths:
     (values '() linklet-e)]
    [`(let* ,bindings ,body)
     (define (path-binding? b)
       (define rhs (cadr b))
       (or (path? rhs)
           (path-for-srcloc? rhs)
           (to-fasl? rhs)))
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
  (lambda (orig-p [for-srcloc? #f])
    (cond
      [(to-fasl? orig-p)
       (define v (force-unfasl orig-p))
       (cond
         [(symbol? v)
          ;; Shortcut for just an uninterned symbol:
          (box v)]
         [else
          (define lifts '())
          (define bstr (s-exp->fasl v
                                    #:handle-fail cannot-fasl
                                    ;; We have to keep uninterned symbols exposed, so they're
                                    ;; fasled with the encloding linklet directory
                                    #:external-lift? (lambda (v)
                                                       (and (symbol? v)
                                                            (not (symbol-interned? v))
                                                            (not (symbol-unreadable? v))
                                                            (begin
                                                              (set! lifts (cons v lifts))
                                                              #t)))))
          (if (null? lifts)
              (box bstr)
              (box (cons bstr (list->vector (reverse lifts)))))])]
      [(symbol? orig-p)
       ;; Must be an uninterned symbol:
       orig-p]
      [else
       (define p (if (path-for-srcloc? orig-p)
                     (path-for-srcloc-path orig-p)
                     orig-p))
       (cond
         [(path? p)
          (or (path->relative-path-elements p)
              (cond
                [(or for-srcloc?
                     (path-for-srcloc? orig-p))
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
    [(box? e)
     (define c (unbox e))
     (to-fasl (box (if (pair? c) (car c) c))
              (if (pair? c) (cdr c) '#())
              (and (not (symbol? c))
                   (or (current-load-relative-directory)
                       (current-directory))))]
    [(symbol? e)
     ;; Must be an uninterned symbol:
     e]
    [(bytes? e) (bytes->path e)]
    [(string? e) e] ; was `path-for-srcloc` on write
    [else (relative-path-elements->path e)]))

(define (force-unfasl tf)
  (cond
    [(not (to-fasl? tf))
     ;; act as identity on other values for the benefit of `raco decompile`
     tf]
    [else
     (define vb (to-fasl-vb tf))
     (define v (unbox vb))
     (cond
       [(bytes? v)
        (define v2 (parameterize ([current-load-relative-directory (to-fasl-wrt tf)])
                     (fasl->s-exp v
                                  #:datum-intern? #t
                                  #:external-lifts (to-fasl-lifts tf))))
        (let loop ()
          (cond
            [(box-cas! vb v v2)
             (set-to-fasl-wrt! tf #f)
             v2]
            [else
             (let ([v (unbox vb)])
               (cond
                 [(bytes? v)
                  ;; must be a spurious CAS failure
                  (loop)]
                 [else
                  ;; other thread beat us to it
                  v]))]))]
       [else
        ;; already forced (or never fasled)
        v])]))

(define (cannot-fasl v)
  (error 'write
         "cannot marshal value that is embedded in compiled code\n  value: ~v"
         v))
