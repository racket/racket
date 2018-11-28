#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "../namespace/core.rkt"
         "../namespace/namespace.rkt"
         "root-expand-context.rkt"
         "context.rkt"
         "def-id.rkt")

;; Helpers to implement the consumer side of `syntax-local-lift-expression`,
;; `syntax-local-lift-module`, etc. These structures are used by `syntax-local-...`
;; functions as communicated through the current expand context.

(provide make-lift-context
         add-lifted!
         get-and-clear-lifts!
         
         make-local-lift
         make-top-level-lift
         wrap-lifts-as-let
         wrap-lifts-as-begin
         get-lifts-as-lists
         
         make-module-lift-context
         get-and-clear-module-lifts!
         add-lifted-module!
         module-lift-context-wrt-phase
         
         make-require-lift-context
         add-lifted-require!
         get-and-clear-require-lifts!
         require-lift-context-wrt-phase
         
         make-to-module-lift-context
         make-shared-module-ends
         to-module-lift-context-end-as-expressions?
         get-and-clear-end-lifts!
         get-and-clear-provide-lifts!
         add-lifted-to-module-provide!
         add-lifted-to-module-end!
         to-module-lift-context-wrt-phase)

;; ----------------------------------------

(define (box-cons! b v)
  (set-box! b (cons v (unbox b))))

(define (box-clear! b)
  (begin0
   (reverse (unbox b))
   (set-box! b null)))

;; ----------------------------------------

(struct lift-context (convert       ; takes a list of ids and rhs to produce a lifted-bind
                      lifts         ; box of list of lifted-binds and maybe other forms
                      module*-ok?)  ; if used to capture module lifts, allow `module*`?
  #:authentic)
(struct lifted-bind (ids keys rhs) #:authentic)

(define (make-lift-context convert #:module*-ok? [module*-ok? #f])
  (lift-context convert (box null) module*-ok?))

(define (add-lifted! lifts ids rhs phase)
  (define-values (lifted-ids lifted) ((lift-context-convert lifts) ids rhs phase))
  (box-cons! (lift-context-lifts lifts) lifted)
  lifted-ids)

(define (get-and-clear-lifts! lifts)
  (box-clear! (lift-context-lifts lifts)))

(define (make-local-lift lift-env counter local-sym)
  (lambda (ids rhs phase)
    (define keys
      (for/list ([id (in-list ids)])
        (define key (add-local-binding! id phase counter #:local-sym local-sym))
        (set-box! lift-env (env-extend (unbox lift-env) key variable))
        key))
    (values ids (lifted-bind ids keys rhs))))

(define (make-top-level-lift ctx)
  (lambda (ids rhs phase)
    ;; Add the namespace's post-expansion scope (i.e., the inside-edge
    ;; scope) so that the binding has a specific phase:
    (define post-scope
      (post-expansion-scope
       (root-expand-context-post-expansion
        (namespace-get-root-expand-ctx
         (expand-context-namespace ctx)))))
    (define tl-ids (for/list ([id (in-list ids)])
                     (add-scope id post-scope)))
    ;; Bind the identifier:
    (define syms (select-defined-syms-and-bind!/ctx tl-ids ctx))
    (values tl-ids (lifted-bind tl-ids syms rhs))))

(define (wrap-lifts-as-let lifts body phase)
  (datum->syntax
   #f
   (for/fold ([body body]) ([lift (in-list (reverse lifts))])
     (unless (lifted-bind? lift)
       (error "non-bindings in `lift-context`"))
     (list (datum->syntax
            (syntax-shift-phase-level core-stx phase)
            'let-values)
           (list (list (lifted-bind-ids lift)
                       (lifted-bind-rhs lift)))
           body))))

(define (wrap-lifts-as-begin lifts body phase
                             #:adjust-form [adjust-form values]
                             #:adjust-body [adjust-body values])
  (datum->syntax
   #f
   (cons (datum->syntax
          (syntax-shift-phase-level core-stx phase)
          'begin)
         (append
          (for/list ([lift (in-list lifts)])
            (adjust-form
             (cond
              [(lifted-bind? lift)
               (datum->syntax
                #f
                (list (datum->syntax
                       (syntax-shift-phase-level core-stx phase)
                       'define-values)
                      (lifted-bind-ids lift)
                      (lifted-bind-rhs lift)))]
              [else lift])))
          (list (adjust-body body))))))

(define (get-lifts-as-lists lifts)
   (for/list ([lift (in-list lifts)])
     (list (lifted-bind-ids lift)
           (lifted-bind-keys lift)
           (lifted-bind-rhs lift))))

;; ----------------------------------------

(struct module-lift-context (wrt-phase    ; phase of target for lifts
                             lifts        ; box of list of lifted
                             module*-ok?) ; whether `module*` is allowed
  #:authentic)

(define (make-module-lift-context phase module*-ok?)
  (module-lift-context phase (box null) module*-ok?))

(define (get-and-clear-module-lifts! module-lifts)
  (box-clear! (module-lift-context-lifts module-lifts)))

(define (add-lifted-module! module-lifts s phase)
  (unless (or (and (module-lift-context? module-lifts)
                   (module-lift-context-module*-ok? module-lifts))
              (and (lift-context? module-lifts)
                   (lift-context-module*-ok? module-lifts)))
    (case (core-form-sym s phase)
      [(module) (void)]
      [(module*)
       (raise-arguments-error 'syntax-local-lift-module
                              "cannot lift `module*' to a top-level context" 
                              "syntax" s)]
      [else 
       (raise-arguments-error 'syntax-local-lift-module
                              "not a `module' declaration" 
                              "syntax" s)]))
  (cond
   [(module-lift-context? module-lifts)
    (box-cons! (module-lift-context-lifts module-lifts) s)]
   [(lift-context? module-lifts)
    ;; Top-level expansion uses a `lift-context` for both, which keeps
    ;; modules and other lifts in order
    (box-cons! (lift-context-lifts module-lifts) s)]
   [else
    (error "internal error: unrecognized lift-context type for module lift")]))

;; ----------------------------------------

(struct require-lift-context (do-require  ; callback to process a lifted require
                              wrt-phase   ; phase of target for lifts
                              requires)   ; records lifted requires
  #:authentic)

(define (make-require-lift-context wrt-phase do-require)
  (require-lift-context do-require wrt-phase (box null)))

(define (get-and-clear-require-lifts! require-lifts)
  (box-clear! (require-lift-context-requires require-lifts)))

(define (add-lifted-require! require-lifts s phase)
  ((require-lift-context-do-require require-lifts) s phase)
  (box-cons! (require-lift-context-requires require-lifts)
             s))

;; ----------------------------------------

(struct to-module-lift-context (wrt-phase   ; phase of target for lifts
                                provides
                                end-as-expressions?
                                ends)
  #:authentic)

(define (make-to-module-lift-context phase
                                     #:shared-module-ends ends
                                     #:end-as-expressions? end-as-expressions?)
  (to-module-lift-context phase
                          (box null) 
                          end-as-expressions?
                          ends))

(define (make-shared-module-ends)
  (box null))

(define (get-and-clear-end-lifts! to-module-lifts)
  (box-clear! (to-module-lift-context-ends to-module-lifts)))

(define (get-and-clear-provide-lifts! to-module-lifts)
  (box-clear! (to-module-lift-context-provides to-module-lifts)))

(define (add-lifted-to-module-provide! to-module-lifts s phase)
  (box-cons! (to-module-lift-context-provides to-module-lifts)
             s))

(define (add-lifted-to-module-end! to-module-lifts s phase)
  (box-cons! (to-module-lift-context-ends to-module-lifts)
             s))
