#lang racket/base
(require (for-syntax racket/base)
         "../common/struct-star.rkt"
         "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "use-site.rkt"
         "context.rkt"
         "main.rkt"
         "log.rkt"
         "free-id-set.rkt"
         "stop-ids.rkt")

(provide add-intdef-scopes
         add-intdef-bindings
         internal-definition-context-frame-id
         
         internal-definition-context?
         syntax-local-make-definition-context
         syntax-local-bind-syntaxes
         internal-definition-context-binding-identifiers
         internal-definition-context-introduce
         internal-definition-context-seal
         identifier-remove-from-definition-context
         internal-definition-context-add-scopes
         internal-definition-context-splice-binding-identifier
         
         make-local-expand-context
         flip-introduction-scopes
         flip-introduction-and-use-scopes
         remove-intdef-use-site-scopes

         intdefs?
         intdefs?-string
         intdefs-or-false?
         intdefs-or-false?-string)

(struct internal-definition-context (frame-id        ; identifies the frame for use-site scopes
                                     outside-edge    ; outside-edge scope for the context
                                     inside-edge     ; inside-edge scope for the context
                                     add-scope?      ; whether the scope is auto-added for expansion
                                     env-mixins      ; bindings for this context: box of list of mix-binding
                                     use-site-scopes ; boxed list of use-site scopes that should be pruned from definition context binders
                                     parent-ctx))    ; parent definition context or #f

(struct env-mixin (id
                   sym
                   value
                   cache)) ; caches addition of binding to an existing environment

;; syntax-local-make-definition-context
(define (syntax-local-make-definition-context [parent-ctx #f] [add-scope? #t])
  (unless (or (not parent-ctx)
              (internal-definition-context? parent-ctx))
    (raise-argument-error 'syntax-local-make-definition-context "(or/c #f internal-definition-context?)" parent-ctx))
  (define ctx (get-current-expand-context 'syntax-local-make-definition-context))
  (define frame-id
    ;; keep parent or context frame-id in order to add use-site scopes to uses of spliced macros
    (or (and parent-ctx (internal-definition-context-frame-id parent-ctx))
        ;; but if the context is 'expression, no splicing is possible
        (and (not (eq? 'expression (expand-context-context ctx)))
             (root-expand-context-frame-id ctx))
        (gensym)))
  (define outside-edge (new-scope 'intdef-outside))
  (define inside-edge (new-scope 'intdef))
  (define def-ctx-scopes (expand-context-def-ctx-scopes ctx))
  (when def-ctx-scopes
    (set-box! def-ctx-scopes (cons inside-edge (cons outside-edge (unbox def-ctx-scopes)))))
  (define use-site-scopes (box '()))
  (internal-definition-context frame-id outside-edge inside-edge add-scope? (box null) use-site-scopes parent-ctx))

;; syntax-local-bind-syntaxes
(define (syntax-local-bind-syntaxes ids s intdef [extra-intdefs '()])
  (unless (and (list? ids)
               (andmap identifier? ids))
    (raise-argument-error 'syntax-local-bind-syntaxes "(listof identifier?)" ids))
  (unless (or (not s) (syntax? s))
    (raise-argument-error 'syntax-local-bind-syntaxes "(or/c syntax? #f)" s))
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'syntax-local-bind-syntaxes "internal-definition-context?" intdef))
  (unless (intdefs? extra-intdefs)
    (raise-argument-error 'syntax-local-bind-syntaxes intdefs?-string extra-intdefs))
  (define ctx (get-current-expand-context 'local-expand))
  (log-expand ctx 'local-bind ids)
  (define phase (expand-context-phase ctx))
  (define all-intdefs (if (null? extra-intdefs)
                          intdef
                          (if (list? extra-intdefs)
                              (cons intdef extra-intdefs)
                              (list intdef extra-intdefs))))
  (define intdef-ids (for/list ([id (in-list ids)])
                       (define pre-id (remove-intdef-use-site-scopes (flip-introduction-scopes id ctx)
                                                                     intdef))
                       (add-intdef-scopes (add-intdef-scopes pre-id intdef #:always? #t)
                                          extra-intdefs)))
  (log-expand ctx 'rename-list intdef-ids)
  (define counter (root-expand-context-counter ctx))
  (define local-sym (and (expand-context-normalize-locals? ctx) 'loc))
  (define syms (for/list ([intdef-id (in-list intdef-ids)])
                 (add-local-binding! intdef-id phase counter
                                     #:frame-id (internal-definition-context-frame-id intdef)
                                     #:local-sym local-sym)))
  (define local-ctx
    (and s
         (let ()
           (define tmp-env (for/fold ([env (expand-context-env ctx)]) ([sym (in-list syms)]
                                                                       [intdef-id (in-list intdef-ids)])
                             (env-extend env sym (local-variable intdef-id))))
           (make-local-expand-context (struct*-copy expand-context ctx
                                                    [env tmp-env])
                                      #:context 'expression
                                      #:intdefs all-intdefs
                                      ;; we're going to evaluate the expansion right away,
                                      ;; so it's not really "local" in that sense, and we
                                      ;; want out-of-context identifiers to be flagged
                                      #:in-local-expand? #f))))
  (define vals
    (cond
     [s
      (define input-s (flip-introduction-scopes (add-intdef-scopes s all-intdefs) ctx))
      (log-expand ctx 'enter-bind)
      (define vals (eval-for-syntaxes-binding 'syntax-local-bind-syntaxes input-s ids local-ctx))
      (log-expand ctx 'exit-bind)
      vals]
     [else
      (for/list ([intdef-id (in-list intdef-ids)]) (local-variable intdef-id))]))
  (define env-mixins (internal-definition-context-env-mixins intdef))
  (set-box! env-mixins (append (for/list ([intdef-id (in-list intdef-ids)]
                                          [sym (in-list syms)]
                                          [val (in-list vals)])
                                 (when local-ctx
                                   (maybe-install-free=id-in-context! val intdef-id phase local-ctx))
                                 (env-mixin intdef-id sym val (make-weak-hasheq)))
                               (unbox env-mixins)))
  (log-expand ctx 'exit-local-bind)
  (for/list ([id (in-list intdef-ids)])
    (flip-introduction-scopes id ctx)))

;; internal-definition-context-binding-identifiers
(define (internal-definition-context-binding-identifiers intdef)
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-binding-identifiers "internal-definition-context?" intdef))
  (for/list ([env-mixin (in-list (unbox (internal-definition-context-env-mixins intdef)))])
    (env-mixin-id env-mixin)))

;; internal-definition-context-introduce
(define (internal-definition-context-introduce intdef s [mode 'flip])
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-introduce "internal-definition-context?" intdef))
  (unless (syntax? s)
    (raise-argument-error 'internal-definition-context-introduce "syntax?" s))
  (define new-s
    (add-intdef-scopes s intdef
                       #:always? #t
                       #:action (case mode
                                  [(add) add-scope]
                                  [(remove) remove-scope]
                                  [(flip) flip-scope]
                                  [else (raise-argument-error
                                         'internal-definition-context-introduce
                                         "(or/c 'add 'remove 'flip)"
                                         mode)])))
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (when ctx (log-expand ctx 'track-syntax 'internal-definition-context-introduce new-s s))
  new-s)

;; internal-definition-context-seal
(define (internal-definition-context-seal intdef) 
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-seal "internal-definition-context?" intdef))
  (void))

;; identifier-remove-from-definition-context
(define (identifier-remove-from-definition-context id intdef)
  (unless (identifier? id)
    (raise-argument-error 'identifier-remove-from-definition-context "identifier?" id))
  (unless (or (internal-definition-context? intdef)
              (and (list? intdef)
                   (andmap internal-definition-context? intdef)))
    (raise-argument-error 'identifier-remove-from-definition-context
                          "(or/c internal-definition-context? (listof internal-definition-context?))"
                          intdef))
  (for/fold ([id id]) ([intdef (in-intdefs intdef)])
    (internal-definition-context-introduce intdef id 'remove)))


;; internal-definition-context-add-scopes
(define (internal-definition-context-add-scopes intdef s)
  (unless (syntax? s)
    (raise-argument-error 'internal-definition-context-add-scopes "syntax?" s))
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-add-scopes
                          "internal-definition-context?"
                          intdef))
  (add-scope
    (add-scope
      s
      (internal-definition-context-inside-edge intdef))
    (internal-definition-context-outside-edge intdef)))

;; internal-definition-context-splice-binding-identifier
(define (internal-definition-context-splice-binding-identifier intdef id)
  (unless (identifier? id)
    (raise-argument-error 'internal-definition-context-splice-binding-identifier "identifier?" id))
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'internal-definition-context-splice-binding-identifier
                          "internal-definition-context?"
                          intdef))

  (remove-intdef-use-site-scopes
    (remove-scope
      (remove-scope
        id
        (internal-definition-context-inside-edge intdef))
      (internal-definition-context-outside-edge intdef))
    intdef))


;; For contract errors:
(define (intdefs? x)
  (or (internal-definition-context? x)
      (and (list? x)
           (andmap internal-definition-context? x))))
(define intdefs?-string
  "(or/c internal-definition-context? (listof internal-definition-context?))")
(define (intdefs-or-false? x)
  (or (not x) (intdefs? x)))
(define intdefs-or-false?-string
  "(or/c internal-definition-context? (listof internal-definition-context?) #f)")

;; Sequence for intdefs provided to `local-expand`
(define-sequence-syntax in-intdefs
  (lambda (stx) (raise-syntax-error #f "only allowed in a `for` form" stx))
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ arg)]
       #'[(d)
          (:do-in
           ([(x) (let ([a arg])
                   (cond
                    [(list? a) (reverse a)]
                    [(not a) null]
                    [else (list a)]))])
           #t
           ([a x])
           (pair? a)
           ([(d) (car a)])
           #t
           #t
           ((cdr a)))]])))

(define (add-intdef-bindings env intdefs)
  (for/fold ([env env]) ([intdef (in-intdefs intdefs)])
    (define parent-ctx (internal-definition-context-parent-ctx intdef))
    (define parent-env (if parent-ctx (add-intdef-bindings env parent-ctx) env))
    (define env-mixins (unbox (internal-definition-context-env-mixins intdef)))
    (let loop ([env parent-env] [env-mixins env-mixins])
      (cond
       [(null? env-mixins) env]
       [else
        (define env-mixin (car env-mixins))
        (or (hash-ref (env-mixin-cache env-mixin) env #f)
            (let ([new-env (env-extend (loop env (cdr env-mixins))
                                       (env-mixin-sym env-mixin)
                                       (env-mixin-value env-mixin))])
              (hash-set! (env-mixin-cache env-mixin) env new-env)
              new-env))]))))

(define (add-intdef-scopes s intdefs
                           #:always? [always? #f]
                           #:action [action add-scope])
  (for/fold ([s s]) ([intdef (in-intdefs intdefs)]
                     #:when (or always?
                                (internal-definition-context-add-scope? intdef)))
    (action s (internal-definition-context-inside-edge intdef))))

;; ----------------------------------------

(define (make-local-expand-context ctx
                                   #:context context
                                   #:phase [phase (expand-context-phase ctx)]
                                   #:intdefs intdefs
                                   #:stop-ids [stop-ids #f]
                                   #:to-parsed-ok? [to-parsed-ok? #f]
                                   #:track-to-be-defined? [track-to-be-defined? #f]
                                   #:keep-#%expression? [keep-#%expression? #t]
                                   #:in-local-expand? [in-local-expand? #t])
  (define same-kind? (or (eq? context
                              (expand-context-context ctx))
                         (and (list? context)
                              (list? (expand-context-context ctx)))))
  (define all-stop-ids (and stop-ids (stop-ids->all-stop-ids stop-ids phase)))
  (define def-ctx-scopes (if (expand-context-def-ctx-scopes ctx)
                             (unbox (expand-context-def-ctx-scopes ctx))
                             null))
  (struct*-copy expand-context ctx
                [context context]
                [env (add-intdef-bindings (expand-context-env ctx)
                                          intdefs)]
                [use-site-scopes
                 #:parent root-expand-context
                 (and (or (eq? context 'module)
                          (eq? context 'module-begin)
                          (list? context))
                      (if (internal-definition-context? intdefs)
                          (internal-definition-context-use-site-scopes intdefs)
                          (or (root-expand-context-use-site-scopes ctx)
                              (box null))))]
                [frame-id #:parent root-expand-context
                          (cond
                            [(internal-definition-context? intdefs)
                             (internal-definition-context-frame-id intdefs)]
                            [(not intdefs) (root-expand-context-frame-id ctx)]
                            ;; Backwards compatible behavior:
                            ;; If there are multiple definition contexts in `intdefs`
                            ;; and if they have different frame IDs, then we conservatively
                            ;; turn on use-site scopes for all frame IDs
                            [(list? intdefs)
                             (for/fold ([frame-id (and (not (eq? 'expression (expand-context-context ctx)))
                                                       (root-expand-context-frame-id ctx))])
                                        ([intdef (in-intdefs intdefs)])
                                (define i-frame-id (internal-definition-context-frame-id intdef))
                                (cond
                                  [(and frame-id i-frame-id (not (eq? frame-id i-frame-id)))
                                   ;; Special ID 'all means "use-site scopes for all expansions"
                                   'all]
                                  [else (or frame-id i-frame-id)]))])]
                [post-expansion #:parent root-expand-context
                                (let ([pe (and same-kind?
                                               (or (pair? context)
                                                   (memq context '(module module-begin top-level)))
                                               (root-expand-context-post-expansion ctx))])
                                  (cond
                                    [(and intdefs (not (null? intdefs)))
                                     (lambda (s)
                                       (add-intdef-scopes (apply-post-expansion pe s) intdefs))]
                                    [else pe]))]
                [scopes
                 (append def-ctx-scopes
                         (expand-context-scopes ctx))]
                [only-immediate? (not stop-ids)] ; def-ctx-scopes is set for the enclosing transformer call
                [to-parsed? (if to-parsed-ok?
                                (expand-context-to-parsed? ctx)
                                #f)]
                [just-once? #f]
                [in-local-expand? in-local-expand?]
                [keep-#%expression? keep-#%expression?]
                [stops (free-id-set phase (or all-stop-ids null))]
                [current-introduction-scopes null]
                [need-eventually-defined (let ([ht (expand-context-need-eventually-defined ctx)])
                                           (cond
                                             [track-to-be-defined?
                                              ;; maintain status quo and propagate tracking
                                              ht]
                                             [ht
                                              ;; keep allowing unbound references, but don't track them
                                              (make-hasheqv)]
                                             [else
                                              ;; keep disallowing unbound references
                                              #f]))]))

;; ----------------------------------------

(define (flip-introduction-scopes s ctx)
  (flip-scopes s (expand-context-current-introduction-scopes ctx)))

(define (flip-introduction-and-use-scopes s ctx)
  (flip-scopes (flip-introduction-scopes s ctx)
               (expand-context-current-use-scopes ctx)))

(define (remove-intdef-use-site-scopes s intdef)
  (define use-sites (internal-definition-context-use-site-scopes intdef))
  (if (syntax? s)
      (remove-scopes s (unbox use-sites))
      (for/list ([id (in-list s)])
        (remove-scopes id (unbox use-sites)))))
