#lang racket/base

(require "context.rkt"
         "definition-context.rkt"
         "main.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../common/struct-star.rkt"
         "../common/contract.rkt"
         "../common/parameter-like.rkt"
         "../namespace/namespace.rkt"
         "../namespace/inspector.rkt"
         "../syntax/weaker-inspector.rkt")

(provide syntax-local-apply-transformer)

(define (transform-syntax-vals f vs)
  (for/list ([v (in-list vs)])
    (if (syntax? v)
        (f v)
        v)))

;; the `local-expand`-like bits are in this function
(define/who (syntax-local-apply-transformer transformer binding-id/insp context intdef-ctx . args)
  (check who procedure? transformer)
  (check who
         (lambda (binding-id/insp)
           (or (eq? binding-id/insp #f)
               (identifier? binding-id/insp)
               (inspector? binding-id/insp)
               (and (list? binding-id/insp)
                    (= 2 (length binding-id/insp))
                    (identifier? (car binding-id/insp))
                    (inspector? (cadr binding-id/insp)))))
         #:contract
         "(or/c identifier? inspector? (list/c internal-definition-context? inspector?) #f)"
         binding-id/insp)
  (check who
         (lambda (context)
           (or (list? context)
               (memq context '(expression top-level module module-begin))))
         #:contract
         "(or/c 'expression 'top-level 'module 'module-begin list?)"
         context)
  (check who
         (lambda (intdef-ctx)
           (or (not intdef-ctx)
               (internal-definition-context? intdef-ctx)))
         #:contract
         "(or/c #f internal-definition-context?)"
         intdef-ctx)

  (define binding-id (or (and (identifier? binding-id/insp)
                              binding-id/insp)
                         (and (pair? binding-id/insp)
                              (car binding-id/insp))))
  (define expander-insp (or (and (inspector? binding-id/insp)
                                 binding-id/insp)
                            (and (pair? binding-id/insp)
                                 (cadr binding-id/insp))
                            (current-module-code-inspector)))

  (define ctx (get-current-expand-context who))

  (define local-ctx (make-local-expand-context
                     ctx
                     #:context context
                     #:intdefs intdef-ctx))

  (define scoped-args (transform-syntax-vals
                       (lambda (s)
                         (add-intdef-scopes
                          (flip-introduction-scopes s ctx)
                          intdef-ctx))
                       args))

  (define scoped-binding-id
    (and binding-id
         (flip-introduction-scopes binding-id ctx)))

  (define output-vals
    (without-expand-context
     (apply-transformer who
                        transformer
                        scoped-binding-id
                        scoped-args
                        local-ctx
                        expander-insp)))

  (define result-vals (transform-syntax-vals
                       (lambda (s) (flip-introduction-scopes s ctx))
                       output-vals))
  
  (apply values result-vals))

;; the macro application-like bits are in this function
(define (apply-transformer who transformer binding-id args ctx expander-insp)
  (define-values (binding insp-of-t)
    (if binding-id
        (let ([binding (resolve+shift binding-id (expand-context-phase ctx)
                                      #:ambiguous-value 'ambiguous
                                      #:immediate? #t)])
          (when (not binding)
            (error who "unbound identifier: ~v" binding-id))
          (let-values ([(t primitive? insp-of-t protected?)
                        (lookup binding ctx binding-id #:out-of-context-as-variable? #t)])
            (values binding insp-of-t)))
        (values #f #f)))

  (define intro-scope (new-scope 'macro))
  (define use-scopes (maybe-create-use-site-scope ctx binding))

  (define (scope-arg s)
    (define intro-s (add-scope s intro-scope))
    (define use-s (add-scopes intro-s use-scopes))
    use-s)

  (define scoped-args (transform-syntax-vals scope-arg args))

  (define transformed-vals
    (apply-transformer-in-context transformer scoped-args ctx
                                  intro-scope use-scopes
                                  binding-id insp-of-t
                                  expander-insp))

  (define (scope-res s)
    (define result-s (flip-scope s intro-scope))
    (define post-s (maybe-add-post-expansion result-s ctx))
    post-s)
  
  (define result-vals (transform-syntax-vals scope-res transformed-vals))

  result-vals)

(define (apply-transformer-in-context transformer args ctx
                                      intro-scope use-scopes
                                      binding-id insp-of-t
                                      expander-insp)

  (define m-ctx (struct*-copy expand-context ctx
                              [current-introduction-scopes (list intro-scope)]
                              [current-use-scopes use-scopes]))

  (define transformed-vals
    (parameterize ([current-namespace (namespace->namespace-at-phase
                                       (expand-context-namespace ctx)
                                       (add1 (expand-context-phase ctx)))])
      (parameterize-like
       #:with ([current-expand-context m-ctx]
               [current-module-code-inspector
                (weaker-inspector insp-of-t expander-insp)])
       (call-with-continuation-barrier
        (lambda ()
          (call-with-values
           (lambda () (apply transformer args))
           list))))))
  
  transformed-vals)


