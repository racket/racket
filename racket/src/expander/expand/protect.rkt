#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../common/module-path.rkt"
         "binding-to-module.rkt")

(provide resolve+shift/extra-inspector
         check-access)

;; Check inspector-based access to a module's definitions; a suitable inspector
;; might be provided by `id`, or the binding might carry an extra inspector
;; (put there via a provide of a rename transformer, where the extra inspector
;; was attached to the identifier in the rename transformer). Return #t if
;; the binding is protected.
(define (check-access b mi id in-s what)
  (define m (module-instance-module mi))
  (cond
    [(and m (not (module-no-protected? m)))
     (define access (or (module-access m) (module-compute-access! m)))
     (define a (hash-ref (hash-ref access (module-binding-phase b) #hasheq())
                         (module-binding-sym b)
                         'unexported))
     (cond
       [(or (eq? a 'unexported) ; not provided => implicitly protected
            (eq? a 'protected))
        (unless (or (inspector-superior? (or (syntax-inspector id) (current-code-inspector))
                                         (namespace-inspector (module-instance-namespace mi)))
                    (and (module-binding-extra-inspector b)
                         (inspector-superior? (module-binding-extra-inspector b)
                                              (namespace-inspector (module-instance-namespace mi)))))
          ;; In the error message, use the original expression `in-s` or
          ;; the symbol protected or defined in the target module ---
          ;; but only if that name is different from `id`, which we'll
          ;; certainly include in the error
          (define complain-id (let ([c-id (or in-s (module-binding-sym b))])
                                (and (not (eq? (if (syntax? c-id) (syntax-content c-id) c-id)
                                               (syntax-content id)))
                                     c-id)))
          (raise-syntax-error #f
                              (format "access disallowed by code inspector to ~a ~a\n  from module: ~a"
                                      a
                                      what
                                      (module-path-index-resolve (namespace-mpi (module-instance-namespace mi))))
                              complain-id id null))
        #t]
       [else #f])]
    [else #f]))

;; Like `resolve+shift`, but follow `free-identifier=?` chains to
;; attach an inspector at the last step in the chain to the
;; resulting binding. Also, check protected access along the way,
;; so that we don't expose an inspector that the reference is not
;; allowed to reach.
(define (resolve+shift/extra-inspector id phase ns)
  (let loop ([id id] [in-s #f])
    (define b (resolve+shift id phase #:immediate? #t))
    (cond
     [(binding-free=id b)
      => (lambda (next-id)
           (when (and (module-binding? b)
                      (not (top-level-module-path-index? (module-binding-module b))))
             (define mi (binding->module-instance b ns phase id))
             (check-access b mi id in-s "provided binding"))
           (define next-b (loop next-id (or in-s id)))
           (cond
            [(not next-b) b]
            [(and (module-binding? next-b)
                  (not (module-binding-extra-inspector next-b))
                  (syntax-inspector id))
             (module-binding-update next-b
                                    #:extra-inspector (syntax-inspector id))]
            [else next-b]))]
     [else b])))
