#lang racket/base
(require "../common/set.rkt"
         "../common/struct-star.rkt"
         "../common/parameter-like.rkt"
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../syntax/original.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/inspector.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "../syntax/track.rkt"
         "../syntax/error.rkt"
         "syntax-id-error.rkt"
         "syntax-implicit-error.rkt"
         "free-id-set.rkt"
         "dup-check.rkt"
         "use-site.rkt"
         "../compile/main.rkt"
         "../eval/top.rkt"
         "../eval/direct.rkt"
         "../namespace/core.rkt"
         "../boot/runtime-primitive.rkt"
         "context.rkt"
         "lift-context.rkt"
         "already-expanded.rkt"
         "liberal-def-ctx.rkt"
         "rename-trans.rkt"
         "allowed-context.rkt"
         "lift-key.rkt"
         "../syntax/debug.rkt"
         "reference-record.rkt"
         "log.rkt"
         "../common/performance.rkt"
         "rebuild.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt"
         "implicit-property.rkt"
         "bindings-arity-error.rkt")

(provide expand
         lookup
         apply-transformer
         
         register-variable-referenced-if-local!
         
         expand/capture-lifts
         expand-transformer
         expand+eval-for-syntaxes-binding
         context->transformer-context
         eval-for-syntaxes-binding
         eval-for-bindings
         raise-bindings-arity-error
         apply-rename-transformer

         keep-properties-only
         keep-properties-only~
         keep-as-needed
         rebuild
         attach-disappeared-transformer-bindings
         increment-binding-layer
         accumulate-def-ctx-scopes
         rename-transformer-target-in-context
         maybe-install-free=id-in-context!
         
         maybe-create-use-site-scope
         maybe-add-post-expansion)

;; ----------------------------------------

;; Main expander dispatch
(define (expand s ctx
                ;; Applying a rename transformer substitutes
                ;; an id without changing `s`
                #:alternate-id [alternate-id #f]
                ;; For expanding an implicit implemented by a rename transformer:
                #:fail-non-transformer [fail-non-transformer #f])
  (log-expand ctx 'visit s)
  (define content (syntax-content s))
  (cond
   [(symbol? content)
    (expand-identifier s ctx alternate-id)]
   [(and (pair? content)
         (syntax-identifier? (car content)))
    (expand-id-application-form s ctx alternate-id
                                #:fail-non-transformer fail-non-transformer)]
   [(or (pair? content)
        (null? content))
    ;; An "application" form that doesn't start with an identifier, so
    ;; use implicit `#%app`
    (expand-implicit '#%app s ctx #f)]
   [(already-expanded? content)
    (expand-already-expanded s ctx)]
   [else
    ;; Anything other than an identifier or parens triggers the
    ;; implicit `#%datum` form
    (expand-implicit '#%datum s ctx #f)]))

;; An identifier by itself (i.e., not after an open parenthesis)
(define (expand-identifier s ctx alternate-id)
  (define id (or alternate-id s))
  (guard-stop
   id ctx s
   (define binding (resolve+shift id (expand-context-phase ctx)
                                  #:ambiguous-value 'ambiguous
                                  #:immediate? #t))
   (log-expand ctx 'resolve id)
   (cond
    [(eq? binding 'ambiguous)
     (raise-ambiguous-error id ctx)]
    [(not binding)
     ;; The implicit `#%top` form handles unbound identifiers
     (expand-implicit '#%top (substitute-alternate-id s alternate-id) ctx s)]
    [else
     ;; Variable or form as identifier macro
     (define-values (t primitive? insp-of-t protected?)
       (lookup binding ctx id
               #:in (and alternate-id s)
               #:out-of-context-as-variable? (expand-context-in-local-expand? ctx)))
     (dispatch t insp-of-t s id ctx binding primitive? protected?)])))

;; An "application" form that starts with an identifier
(define (expand-id-application-form s ctx alternate-id
                                    #:fail-non-transformer fail-non-transformer)
  (define id (or alternate-id (car (syntax-e s))))
  (guard-stop
   id ctx s
   (define binding (resolve+shift id (expand-context-phase ctx)
                                  #:ambiguous-value 'ambiguous
                                  #:immediate? #t))
   (log-expand ctx 'resolve id)
   (cond
     [(eq? binding 'ambiguous)
      (when fail-non-transformer (fail-non-transformer))
      (raise-ambiguous-error id ctx)]
     [(not binding)
      (when fail-non-transformer (fail-non-transformer))
      ;; The `#%app` binding might do something with unbound ids
      (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
    [else
     ;; Find out whether it's bound as a variable, syntax, or core form
     (define-values (t primitive? insp-of-t protected?)
       (lookup binding ctx id
               #:in (and alternate-id (car (syntax-e s)))
               #:out-of-context-as-variable? (expand-context-in-local-expand? ctx)))
     (cond
       [(variable? t)
        (when fail-non-transformer (fail-non-transformer))
        ;; Not as syntax or core form, so use implicit `#%app`
        (expand-implicit '#%app (substitute-alternate-id s alternate-id) ctx id)]
       [else
        ;; Syntax or core form as "application"
        (dispatch t insp-of-t s id ctx binding primitive? protected?
                  #:fail-non-transformer fail-non-transformer)])])))

;; Handle an implicit: `#%app`, `#%top`, or `#%datum`; this is similar
;; to handling an id-application form, but there are several little
;; differences: the binding must be a core form or transformer,
;; an implicit `#%top` is handled specially, and so on
(define (expand-implicit sym s ctx trigger-id)
  (cond
    [(expand-context-only-immediate? ctx)
     (log-expand ctx 'stop/return s)
     s]
    [else
     (define id (datum->syntax s sym))
     (guard-stop
      id ctx s
      (define b (resolve+shift id (expand-context-phase ctx)
                               #:ambiguous-value 'ambiguous
                               #:immediate? #t))
      (log-expand ctx 'resolve id)
      (cond
        [(eq? b 'ambiguous)
         (raise-ambiguous-error id ctx)]
        [else
         (define-values (t primitive? insp-of-t protected?)
           (if b (lookup b ctx id) (values #f #f #f #f)))
         (cond
           [(transformer? t)
            (define fail-non-transformer
              ;; Make sure a rename transformer eventually leads to syntax
              (and (rename-transformer? t)
                   (lambda ()
                     (raise-syntax-implicit-error s sym trigger-id ctx))))
            (dispatch-transformer t insp-of-t (make-explicit ctx sym s) id ctx b
                                  #:fail-non-transformer fail-non-transformer)]
           [(core-form? t)
            (cond
              [(and (eq? sym '#%top)
                    (eq? (core-form-name t) '#%top)
                    (expand-context-in-local-expand? ctx))
               (dispatch-implicit-#%top-core-form t s ctx)]
              [else
               (dispatch-core-form t (make-explicit ctx sym s) ctx)])]
           [else
            (define tl-id
              (and (eq? sym '#%top)
                   (root-expand-context-top-level-bind-scope ctx)
                   (add-scope s (root-expand-context-top-level-bind-scope ctx))))
            (define tl-b (and tl-id (resolve tl-id (expand-context-phase ctx))))
            (cond
              [tl-b
               ;; Special case: the identifier is not bound and its scopes don't
               ;; have a binding for `#%top`, but it's bound temporaily for compilation;
               ;; treat the identifier as a variable reference
               (if (and (expand-context-to-parsed? ctx)
                        (free-id-set-empty? (expand-context-stops ctx)))
                   (parsed-id tl-id tl-b #f)
                   (begin
                     (log-expand* ctx ['variable tl-id] ['return tl-id])
                     tl-id))]
              [else
               (raise-syntax-implicit-error s sym trigger-id ctx)])])]))]))

;; An expression that is already fully expanded via `local-expand-expression`
(define (expand-already-expanded s ctx)
  (define ae (syntax-e s))
  (define exp-s (already-expanded-s ae))
  (when (or (syntax-any-macro-scopes? s)
            (not (eq? (expand-context-binding-layer ctx)
                      (already-expanded-binding-layer ae)))
            (and (parsed? exp-s)
                 (not (and (expand-context-to-parsed? ctx)
                           (free-id-set-empty? (expand-context-stops ctx))))))
    (raise-syntax-error #f
                        (string-append "expanded syntax not in its original lexical context;\n"
                                       " extra bindings or scopes in the current context")
                        (and (not (parsed? exp-s)) exp-s)))
  (cond
    [(expand-context-only-immediate? ctx)
     (log-expand ctx 'stop/return s)
     s]
    [(parsed? exp-s) exp-s]
    [else
     (define result-s (syntax-track-origin exp-s s))
     (log-expand ctx 'opaque-expr result-s) ;; FIXME: or exp-s?
     (if (and (expand-context-to-parsed? ctx)
              (free-id-set-empty? (expand-context-stops ctx)))
         (expand result-s ctx) ; fully expanded to compiled
         result-s)]))

(define (make-explicit ctx sym s)
  (define insp (current-module-code-inspector))
  (define sym-s (immediate-datum->syntax s sym s
                                         (if (syntax-has-property? s original-property-sym)
                                             original-implicit-made-explicit-properties
                                             implicit-made-explicit-properties)
                                         insp))
  (define new-s (immediate-datum->syntax s (cons sym-s s) s
                                         (syntax-props s)
                                         insp))
  (log-expand ctx 'tag2 new-s s)
  new-s)

;; ----------------------------------------

;; Expand `s` given that the value `t` of the relevant binding,
;; where `t` is either a core form, a macro transformer, some
;; other compile-time value (which is an error), or a token
;; indicating that the binding is a run-time variable
(define (dispatch t insp-of-t s id ctx binding primitive? protected?
                  #:fail-non-transformer [fail-non-transformer #f])
  (cond
   [(core-form? t)
    (dispatch-core-form t s ctx)]
   [(transformer? t)
    (dispatch-transformer t insp-of-t s id ctx binding
                          #:fail-non-transformer fail-non-transformer)]
   [(variable? t)
    (dispatch-variable t s id ctx binding primitive? protected?)]
   [else
    ;; Some other compile-time value:
    (raise-syntax-error #f "illegal use of syntax" s
                        #f null
                        (format "\n  value at phase ~s: ~e"
                                (add1 (expand-context-phase ctx))
                                t))]))

;; Call a core-form expander (e.g., `lambda`)
(define (dispatch-core-form t s ctx)
  (cond
   [(expand-context-only-immediate? ctx)
    (log-expand ctx 'stop/return s)
    s]
   [(expand-context-observer ctx)
    (log-expand ctx 'enter-prim s)
    (define result-s ((core-form-expander t) s ctx))
    (log-expand ctx 'exit-prim/return (extract-syntax result-s))
    result-s]
   [else
    ;; As previous case, but as a tail call:
    ((core-form-expander t) s ctx)]))

;; Special favor to `local-expand` from `expand-implicit`: call
;; `#%top` form without making `#%top` explicit in the form
(define (dispatch-implicit-#%top-core-form t s ctx)
  (log-expand ctx 'enter-prim s)
  (define result-s ((core-form-expander t) s ctx #t))
  (log-expand ctx 'exit-prim/return result-s)
  result-s)

;; Call a macro expander, taking into account whether it works
;; in the current context, whether to expand just once, etc.
(define (dispatch-transformer t insp-of-t s id ctx binding
                              #:fail-non-transformer fail-non-transformer)
  (cond
   [(not-in-this-expand-context? t ctx)
    (define adj-s (avoid-current-expand-context (substitute-alternate-id s id) t ctx))
    (log-expand ctx 'tag/context adj-s)
    (expand adj-s ctx)]
   [(and (expand-context-parsing-expanded? ctx)
         ;; It's ok to have a rename transformer whose target
         ;; is a primitive form, so if it's a rename transformer,
         ;; delay the check for another step
         (not (rename-transformer? t)))
    (raise-syntax-error #f
                        "encountered a macro binding in form that should be fully expanded"
                        s)]
   [(rename-transformer? t)
    (cond
      [(expand-context-just-once? ctx) s]
      [else
       (define alt-id (apply-rename-transformer t id ctx))
       (log-expand ctx 'rename-transformer alt-id)
       (expand s ctx
               #:alternate-id alt-id
               #:fail-non-transformer fail-non-transformer)])]
   [else
    ;; Apply transformer and expand again
    (define-values (exp-s re-ctx)
      (apply-transformer t insp-of-t s id ctx binding))
    (cond
      [(expand-context-just-once? ctx) exp-s]
      [else (expand exp-s re-ctx)])]))

;; Handle the expansion of a variable to itself
(define (dispatch-variable t s id ctx binding primitive? protected?)
  (cond
   [(expand-context-only-immediate? ctx)
    (log-expand ctx 'stop/return id)
    id]
   [else
    (log-expand ctx 'variable s id)
    ;; A reference to a variable expands to itself
    (register-variable-referenced-if-local! binding ctx)
    ;; If the variable is locally bound, replace the use's scopes with the binding's scopes
    (define result-s (substitute-variable id t #:no-stops? (free-id-set-empty-or-just-module*? (expand-context-stops ctx))))
    (cond
      [(and (expand-context-to-parsed? ctx)
            (free-id-set-empty? (expand-context-stops ctx)))
       (define prop-s (keep-properties-only~ result-s))
       (define insp (syntax-inspector result-s))
       (if primitive?
           (parsed-primitive-id prop-s binding insp)
           (parsed-id prop-s binding insp))]
      [else
       (define protected-result-s (if protected?
                                      (syntax-property result-s 'protected #t)
                                      result-s))
       (log-expand ctx 'return protected-result-s)
       protected-result-s])]))

;; ----------------------------------------

;; Given a macro transformer `t`, apply it --- adding appropriate
;; scopes to represent the expansion step; the `insp-of-t` inspector
;; is the inspector of the module that defines `t`, which gives its
;; privilege for accessing bindings
(define (apply-transformer t insp-of-t s id ctx binding
                           #:origin-id [origin-id #f])
  (performance-region
   ['expand '_ 'macro]

   (log-expand ctx 'enter-macro s s)
   (define intro-scope (new-scope 'macro))
   (define intro-s (flip-scope s intro-scope))
   ;; In a definition context, we need use-site scopes
   (define use-scopes (maybe-create-use-site-scope ctx binding))
   (define use-s (add-scopes intro-s use-scopes))
   ;; Prepare to accumulate definition contexts created by the transformer
   (define def-ctx-scopes (box null))
   
   ;; Call the transformer; the current expansion context may be needed
   ;; for `syntax-local-....` functions, and we may accumulate scopes from
   ;; definition contexts created by the transformer
   (define transformed-s
     (apply-transformer-in-context t use-s ctx insp-of-t
                                   intro-scope use-scopes def-ctx-scopes
                                   id))
   
   ;; Flip the introduction scope
   (define result-s (flip-scope transformed-s intro-scope))
   ;; In a definition context, we need to add the inside-edge scope to
   ;; any expansion result
   (define post-s (maybe-add-post-expansion result-s ctx))
   ;; Track expansion:
   (define tracked-s (syntax-track-origin post-s use-s (or origin-id (if (syntax-identifier? s) s (car (syntax-e s))))))
   (log-expand ctx 'exit-macro tracked-s post-s)
   (values tracked-s
           (accumulate-def-ctx-scopes ctx def-ctx-scopes))))

;; With all the pre-call scope work done and post-call scope work in
;; the continuation, actually call the transformer function in the
;; appropriate context
(define (apply-transformer-in-context t use-s ctx insp-of-t
                                      intro-scope use-scopes def-ctx-scopes
                                      id)
  (log-expand ctx 'macro-pre-x use-s)
  (define confine-def-ctx-scopes?
    (not (or (expand-context-only-immediate? ctx)
             (not (free-id-set-empty-or-just-module*? (expand-context-stops ctx))))))
  (define accum-ctx
    (if (and confine-def-ctx-scopes?
             (expand-context-def-ctx-scopes ctx)
             (not (null? (unbox (expand-context-def-ctx-scopes ctx)))))
        (accumulate-def-ctx-scopes ctx (expand-context-def-ctx-scopes ctx))
        ctx))
  (define m-ctx (struct*-copy expand-context accum-ctx
                              [current-introduction-scopes (list intro-scope)]
                              [current-use-scopes use-scopes]
                              [def-ctx-scopes
                                (if confine-def-ctx-scopes?
                                    ;; Can confine tracking to this call
                                    def-ctx-scopes
                                    ;; Keep old def-ctx-scopes box, so that we don't
                                    ;; lose them at the point where expansion stops
                                    (expand-context-def-ctx-scopes ctx))]))
  (define transformed-s
    (parameterize ([current-namespace (namespace->namespace-at-phase
                                       (expand-context-namespace ctx)
                                       (add1 (expand-context-phase ctx)))])
      (parameterize-like
       #:with ([current-expand-context m-ctx]
               [current-module-code-inspector (or insp-of-t #;(current-module-code-inspector))])
       (call-with-continuation-barrier
        (lambda ()
          ;; Call the transformer!
          ((transformer->procedure t) use-s))))))
  (log-expand ctx 'macro-post-x transformed-s use-s)
  (unless (syntax? transformed-s)
    (raise-arguments-error (syntax-e id)
                           "received value from syntax expander was not syntax"
                           "received" transformed-s))
  transformed-s)

(define (maybe-create-use-site-scope ctx binding)
  (cond
   [(and (root-expand-context-use-site-scopes ctx)
         (or
          ;; conservatively use a use-site scope when the origin of the
          ;; transformer is unknown (as in some uses of
          ;; syntax-local-apply-transformer)
          (not binding)
          (matching-frame? (root-expand-context-frame-id ctx)
                           (binding-frame-id binding))))
    ;; We're in a recursive definition context where use-site scopes
    ;; are needed, so create one, record it, and add to the given
    ;; syntax
    (define sc (new-scope 'use-site))
    (define b (root-expand-context-use-site-scopes ctx))
    (set-box! b (cons sc (unbox b)))

    (define def-ctx-b (expand-context-def-ctx-scopes ctx))
    (when def-ctx-b
      (set-box! def-ctx-b (cons sc (unbox def-ctx-b))))

    (list sc)]
   [else null]))

(define (matching-frame? current-frame-id bind-frame-id)
  (and current-frame-id
       (or (eq? current-frame-id bind-frame-id)
           (eq? current-frame-id 'all))))

(define (maybe-add-post-expansion s ctx)
  ;; We may be in a definition context where, say, an inside-edge scope
  ;; needs to be added to any immediate macro expansion; that way,
  ;; if the macro expands to a definition form, the binding will be
  ;; in the definition context's scope. The sepcific action depends
  ;; on the expansion context.
  (apply-post-expansion (root-expand-context-post-expansion ctx)
                        s))

(define (accumulate-def-ctx-scopes ctx def-ctx-scopes)
  ;; Move any accumulated definition-context scopes to the `scopes`
  ;; list for further expansion:
  (if (null? (unbox def-ctx-scopes))
      ctx
      (struct*-copy expand-context ctx
                    [scopes (append (unbox def-ctx-scopes)
                                    (expand-context-scopes ctx))])))

;; ----------------------------------------

;; "Apply" a rename transformer, replacing it with its target.
(define (apply-rename-transformer t id ctx)
  (define target-id (rename-transformer-target-in-context t ctx))
  ;; Adding a macro-introduction scope doesn't affect scoping at all, but it can affect
  ;; whether the result is `syntax-original?`
  (define intro-scope (new-scope 'macro))
  (define intro-id (add-scope target-id intro-scope))
  (syntax-track-origin (transfer-srcloc intro-id id) id id))

;; ----------------------------------------

;; Helper to lookup a binding in an expansion context
(define (lookup b ctx id
                #:in [in-s #f]
                #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-lift-envs ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id
                  #:in in-s
                  #:out-of-context-as-variable? out-of-context-as-variable?))

(define-syntax-rule (guard-stop id ctx s otherwise ...)
  (cond
    [(and (not (free-id-set-empty? (expand-context-stops ctx)))
          (free-id-set-member? (expand-context-stops ctx)
                               (expand-context-phase ctx)
                               id))
     (log-expand* ctx ['resolve id] ['stop/return s])
     s]
    [else
     otherwise ...]))

(define (substitute-alternate-id s alternate-id)
  (cond
   [(not alternate-id) s]
   [(syntax-identifier? s) (syntax-track-origin alternate-id s)]
   [else (syntax-track-origin (datum->syntax
                               s
                               (cons alternate-id
                                     (cdr (syntax-e s)))
                               s)
                              s)]))

(define (register-variable-referenced-if-local! binding ctx)
  ;; If the binding's frame has a reference record, then register
  ;; the use for the purposes of `letrec` splitting
  (when (and (local-binding? binding)
             (reference-record? (binding-frame-id binding))
             (not (expand-context-parsing-expanded? ctx)))
    (reference-record-used! (binding-frame-id binding) (local-binding-key binding))))

;; ----------------------------------------

;; Expand `s` and capture lifted expressions, combining expanded term
;; and lifts using `begin` or `let` wrapper
(define (expand/capture-lifts s ctx
                              #:expand-lifts? [expand-lifts? #f]
                              #:begin-form? [begin-form? #f]
                              #:lift-key [lift-key (generate-lift-key)]
                              #:always-wrap? [always-wrap? #f])
  (define context (expand-context-context ctx))
  (define phase (expand-context-phase ctx))
  (define local? (not begin-form?)) ;; see "[*]" below
  ;; Expand `s`, but loop to handle lifted expressions
  (let loop ([s s] [always-wrap? always-wrap?] [ctx ctx])
    (define lift-env (and local? (box empty-env)))
    (define lift-ctx (make-lift-context
                      (if local?
                          (make-local-lift lift-env
                                           (root-expand-context-counter ctx)
                                           (and (expand-context-normalize-locals? ctx) 'lift))
                          (make-top-level-lift ctx))
                      #:module*-ok? (and (not local?) (eq? context 'module))))
    (define capture-ctx (struct*-copy expand-context ctx
                                      [lift-key #:parent root-expand-context lift-key]
                                      [lifts lift-ctx]
                                      [lift-envs (if local?
                                                     (cons lift-env
                                                           (expand-context-lift-envs ctx))
                                                     (expand-context-lift-envs ctx))]
                                      [module-lifts (if (or local?
                                                            (not (memq context '(top-level module))))
                                                        (expand-context-module-lifts ctx)
                                                        lift-ctx)]))
    (define rebuild-s (keep-properties-only s))
    (define exp-s (expand s capture-ctx))
    (define lifts (get-and-clear-lifts! (expand-context-lifts capture-ctx)))
    (define with-lifts-s
      (cond
       [(or (pair? lifts) always-wrap?)
        (cond
         [(expand-context-to-parsed? ctx)
          (unless expand-lifts? (error "internal error: to-parsed mode without expanding lifts"))
          (wrap-lifts-as-parsed-let lifts exp-s rebuild-s ctx (lambda (rhs rhs-ctx) (loop rhs #f rhs-ctx)))]
         [else
          (if begin-form?
              (wrap-lifts-as-begin lifts exp-s phase)
              (wrap-lifts-as-let lifts exp-s phase))])]
       [else exp-s]))
    (cond
     [(or (not expand-lifts?) (null? lifts) (expand-context-to-parsed? ctx))
      ;; Expansion is done
      with-lifts-s]
     [else
      ;; Expand again...
      (log-expand ctx 'letlift-loop with-lifts-s)
      (loop with-lifts-s #f ctx)])))

;; [*] Although `(memq context '(top-level module))` makes more sense
;;     than `(not begin-form?)`, the latter was used historically; the
;;     implementation of `typed/require` currently depends on that
;;     choice, because it expands in 'expression mode to obtain forms
;;     that are splcied into a module context --- leading to an
;;     out-of-context definition error if the historical choice is not
;;     preserved.

;; Expand `s` as a compile-time expression relative to the current
;; expansion context
(define (expand-transformer s ctx
                            #:context [context 'expression]
                            #:begin-form? [begin-form? #f]
                            #:expand-lifts? [expand-lifts? #t]
                            #:lift-key [lift-key (generate-lift-key)]
                            #:always-wrap? [always-wrap? #f]
                            #:keep-stops? [keep-stops? #f])
  (performance-region
   ['expand 'transformer]

   (define trans-ctx (context->transformer-context ctx context
                                                   #:keep-stops? keep-stops?))
   (expand/capture-lifts s trans-ctx
                         #:expand-lifts? expand-lifts?
                         #:begin-form? begin-form?
                         #:lift-key lift-key
                         #:always-wrap? always-wrap?)))

(define (context->transformer-context ctx [context 'expression]
                                      #:keep-stops? [keep-stops? #f])
  (define phase (add1 (expand-context-phase ctx)))
  (define ns (namespace->namespace-at-phase (expand-context-namespace ctx)
                                            phase))
  (namespace-visit-available-modules! ns phase) ; redundant?
  (struct*-copy expand-context ctx
                [context context]
                [scopes null]
                [phase phase]
                [namespace ns]
                [env empty-env]
                [only-immediate? (and keep-stops? (expand-context-only-immediate? ctx))]
                [in-local-expand? #f]
                [stops (if keep-stops?
                           (expand-context-stops ctx)
                           empty-free-id-set)]
                [def-ctx-scopes #f]
                [post-expansion #:parent root-expand-context #f]))

;; Expand and evaluate `s` as a compile-time expression, ensuring that
;; the number of returned values matches the number of target
;; identifiers; return the expanded form as well as its values
(define (expand+eval-for-syntaxes-binding who rhs ids ctx
                                          #:log-next? [log-next? #t]
                                          #:wrap [wrap #f])
  (define exp-rhs (expand-transformer rhs (as-named-context ctx ids)))
  (define phase (add1 (expand-context-phase ctx)))
  (define parsed-rhs (if (expand-context-to-parsed? ctx)
                         exp-rhs
                         (expand exp-rhs (context->transformer-context
                                          (as-to-parsed-context ctx)))))
  (when log-next? (log-expand ctx 'next))
  (values exp-rhs
          parsed-rhs
          (eval-for-bindings who
                             ids
                             parsed-rhs
                             phase
                             (namespace->namespace-at-phase
                              (expand-context-namespace ctx)
                              phase)
                             ctx
                             #:wrap wrap)))

;; Expand and evaluate `s` as a compile-time expression, returning
;; only the compile-time values
(define (eval-for-syntaxes-binding who rhs ids ctx)
  (define-values (exp-rhs parsed-rhs vals)
    (expand+eval-for-syntaxes-binding who rhs ids ctx))
  vals)

;; Expand and evaluate `s` as an expression in the given phase;
;; ensuring that the number of returned values matches the number of
;; target identifiers; return the values
(define (eval-for-bindings who ids p phase ns ctx
                           #:wrap [wrap #f])
  (define compiled (if (can-direct-eval? p ns (root-expand-context-self-mpi ctx))
                       #f
                       (compile-single p (make-compile-context
                                          #:namespace ns
                                          #:phase phase))))
  (define vals
    (call-with-values (lambda ()
                        (call-with-continuation-barrier
                         (lambda ()
                           (parameterize ([current-namespace ns]
                                          [eval-jit-enabled #f])
                             (parameterize-like
                              #:with ([current-expand-context ctx])
                              (if compiled
                                  (if wrap
                                      (wrap (lambda () (eval-single-top compiled ns)))
                                      (eval-single-top compiled ns))
                                  (let ([self-mpi (root-expand-context-self-mpi ctx)])
                                    (if wrap
                                        (wrap (lambda () (direct-eval p ns self-mpi)))
                                        (direct-eval p ns self-mpi)))))))))
      list))
  (unless (or wrap (= (length vals) (length ids)))
    (raise-bindings-arity-error who ids vals))
  vals)

;; ----------------------------------------

(define (keep-properties-only s)
  (datum->syntax #f 'props s s))

;; For cases where we don't actually keep properties, because
;; the compiler doesn't currently use them:
(define (keep-properties-only~ s)
  #f)

;; Drop the `syntax-e` part of `s`, and also drop its scopes when
;; producing a parsed result, producing a result suitable for use with
;; `rebuild`, including in a `parsed` record, or to provide a form
;; name for error reporting. In fact, when producing a parsed value
;; and `keep-for-parsed?` and `keep-for-error?` are both false, then
;; keep nothing (because the compiler isn't going to use it).
;; Dropping references in this way helps the
;; GC not retain too much of an original syntax object in the process
;; of expanding it, which can matter for deeply nested expansions.
(define (keep-as-needed ctx s
                        #:for-track? [for-track? #f]
                        #:keep-for-parsed? [keep-for-parsed? #f]
                        #:keep-for-error? [keep-for-error? #f])
  (define d (syntax-e s))
  (define keep-e (cond
                  [(symbol? d) d]
                  [(and (pair? d) (syntax-identifier? (car d))) (syntax-e (car d))]
                  [else #f]))
  (cond
   [(expand-context-to-parsed? ctx)
    (and (or keep-for-parsed? keep-for-error?) (datum->syntax #f keep-e s s))]
   [(and for-track? (pair? d) keep-e)
    ;; Synthesize form to preserve just source and properties for tracking
    ;; without affecting the identifier that is kept in 'origin
    (datum->syntax #f (list (car d)) s s)]
   [else (datum->syntax s keep-e s s)]))

(define (attach-disappeared-transformer-bindings s trans-idss)
   (cond
    [(null? trans-idss) s]
    [else
     (syntax-property s
                      'disappeared-binding
                      (append (apply append trans-idss)
                              (or (syntax-property s 'disappeared-binding)
                                  null)))]))

;; Generate a fresh binding-layer identity if `ids` contains any
;; identifiers
(define (increment-binding-layer ids ctx layer-val)
  (if (let loop ([ids ids])
        (or (identifier? ids)
            (and (pair? ids)
                 (or (loop (car ids)) (loop (cdr ids))))))
      layer-val
      (expand-context-binding-layer ctx)))

;; Wrap lifted forms in a `let` for a mode where we're generating a
;; parsed result. The body has already been parsed, and the left-hand
;; sides already have bindings. We need to parse the right-hand sides
;; as a series of nested `lets`.
(define (wrap-lifts-as-parsed-let lifts exp-s rebuild-s ctx parse-rhs)
  (define idss+keyss+rhss (get-lifts-as-lists lifts))
  (let lets-loop ([idss+keyss+rhss idss+keyss+rhss] [rhs-ctx ctx])
    (cond
     [(null? idss+keyss+rhss) exp-s]
     [else
      (define ids (caar idss+keyss+rhss))
      (define keys (cadar idss+keyss+rhss))
      (define rhs (caddar idss+keyss+rhss))
      (define exp-rhs (parse-rhs rhs rhs-ctx))
      (parsed-let-values
       rebuild-s
       (list ids)
       (list (list keys exp-rhs))
       (list
        (lets-loop (cdr idss+keyss+rhss)
                   (struct*-copy expand-context rhs-ctx
                                 [env (for/fold ([env (expand-context-env rhs-ctx)]) ([id (in-list ids)]
                                                                                      [key (in-list keys)])
                                        (env-extend env key (local-variable id)))]))))])))

;; A rename transformer can have a `prop:rename-transformer` property
;; as a function, and that fnuction might want to use
;; `syntax-local-value`, etc.
(define (rename-transformer-target-in-context t ctx)
  (parameterize-like
   #:with ([current-expand-context ctx])
   (rename-transformer-target t)))

;; In case the rename-transformer has a callback, ensure that the
;; current expansion context is available while installing a
;; `free-identifier=?` equivalence
(define (maybe-install-free=id-in-context! val id phase ctx)
  (when (rename-transformer? val)
    (parameterize-like
     #:with ([current-expand-context ctx])
     (maybe-install-free=id! val id phase))))

;; Transfer the original ID's source location, if any, when expanding
;; a reference to a rename transformer
(define (transfer-srcloc new-s old-s)
  (define srcloc (syntax-srcloc old-s))
  (if srcloc
      (struct-copy syntax new-s
                   [srcloc srcloc])
      new-s))
