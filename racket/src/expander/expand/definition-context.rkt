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
         
         make-local-expand-context
         flip-introduction-scopes)

(struct internal-definition-context (frame-id      ; identifies the frame for use-site scopes
                                     scope         ; scope that represents the context
                                     add-scope?    ; whether the scope is auto-added for expansion
                                     env-mixins))  ; bindings for this context: box of list of mix-binding

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
  (define frame-id (or (root-expand-context-frame-id ctx)
                       (and parent-ctx (internal-definition-context-frame-id parent-ctx))
                       (gensym)))
  (define sc (new-scope 'intdef))
  (define def-ctx-scopes (expand-context-def-ctx-scopes ctx))
  (unless def-ctx-scopes (error "internal error: no box to accumulate definition-context scopes"))
  (set-box! def-ctx-scopes (cons sc (unbox def-ctx-scopes)))
  (internal-definition-context frame-id sc add-scope? (box null)))

;; syntax-local-bind-syntaxes
(define (syntax-local-bind-syntaxes ids s intdef)
  (unless (and (list? ids)
               (andmap identifier? ids))
    (raise-argument-error 'syntax-local-bind-syntaxes "(listof identifier?)" ids))
  (unless (or (not s) (syntax? s))
    (raise-argument-error 'syntax-local-bind-syntaxes "(or/c syntax? #f)" s))
  (unless (internal-definition-context? intdef)
    (raise-argument-error 'syntax-local-bind-syntaxes "internal-definition-context?" intdef))
  (define ctx (get-current-expand-context 'local-expand))
  (log-expand ctx 'local-bind ids)
  (define phase (expand-context-phase ctx))
  (define intdef-env (add-intdef-bindings (expand-context-env ctx)
                                          intdef))
  (define intdef-ids (for/list ([id (in-list ids)])
                       (define pre-id (remove-use-site-scopes (flip-introduction-scopes id ctx)
                                                              ctx))
                       (add-intdef-scopes pre-id intdef #:always? #t)))
  (log-expand ctx 'rename-list intdef-ids)
  (define syms (for/list ([intdef-id (in-list intdef-ids)])
                 (add-local-binding! intdef-id phase (root-expand-context-counter ctx)
                                     #:frame-id (internal-definition-context-frame-id intdef))))
  (define vals
    (cond
     [s
      (define input-s (flip-introduction-scopes (add-intdef-scopes s intdef #:always? #t)
                                                ctx))
      (define tmp-env (for/fold ([env intdef-env]) ([sym (in-list syms)])
                        (hash-set env sym variable)))
      (log-expand ctx 'enter-bind)
      (define vals
        (eval-for-syntaxes-binding input-s ids
                                   (make-local-expand-context (struct*-copy expand-context ctx
                                                                            [env tmp-env])
                                                              #:context 'expression
                                                              #:intdefs intdef)))
      (log-expand ctx 'exit-bind)
      vals]
     [else
      (for/list ([id (in-list ids)]) variable)]))
  (define env-mixins (internal-definition-context-env-mixins intdef))
  (set-box! env-mixins (append (for/list ([intdef-id (in-list intdef-ids)]
                                          [sym (in-list syms)]
                                          [val (in-list vals)])
                                 (maybe-install-free=id-in-context! val intdef-id phase ctx)
                                 (env-mixin intdef-id sym val (make-weak-hasheq)))
                               (unbox env-mixins)))
  (log-expand ctx 'exit-local-bind))

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
  (add-intdef-scopes s intdef
                     #:action (case mode
                                [(add) add-scope]
                                [(remove) remove-scope]
                                [(flip) flip-scope]
                                [else (raise-argument-error
                                       internal-definition-context-introduce
                                       "(or/c 'add 'remove 'flip)"
                                       mode)])))

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
    (define env-mixins (unbox (internal-definition-context-env-mixins intdef)))
    (let loop ([env env] [env-mixins env-mixins])
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
    (action s (internal-definition-context-scope intdef))))

;; ----------------------------------------

(define (make-local-expand-context ctx
                                   #:context context
                                   #:phase [phase (expand-context-phase ctx)]
                                   #:intdefs intdefs
                                   #:stop-ids [stop-ids #f]
                                   #:to-parsed-ok? [to-parsed-ok? #f]
                                   #:track-to-be-defined? [track-to-be-defined? #f])
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
                      (or (root-expand-context-use-site-scopes ctx)
                          (box null)))]
                [frame-id #:parent root-expand-context
                          ;; If there are multiple definition contexts in `intdefs`
                          ;; and if they have different frame IDs, then we conservatively
                          ;; turn on use-site scopes for all frame IDs
                          (for/fold ([frame-id (root-expand-context-frame-id ctx)]) ([intdef (in-intdefs intdefs)])
                            (define i-frame-id (internal-definition-context-frame-id intdef))
                            (cond
                             [(and frame-id i-frame-id (not (eq? frame-id i-frame-id)))
                              ;; Special ID 'all means "use-site scopes for all expansions"
                              'all]
                             [else (or frame-id i-frame-id)]))]
                [post-expansion-scope
                 #:parent root-expand-context
                 (if intdefs
                     (new-scope 'macro) ; placeholder; action uses `indefs`
                     (and same-kind?
                          (memq context '(module module-begin top-level))
                          (root-expand-context-post-expansion-scope ctx)))]
                [post-expansion-scope-action
                 (if intdefs
                     (lambda (s placeholder-sc)
                       (add-intdef-scopes s intdefs))
                     (expand-context-post-expansion-scope-action ctx))]
                [scopes
                 (append def-ctx-scopes
                         (expand-context-scopes ctx))]
                [only-immediate? (not stop-ids)] ; def-ctx-scopes is set for the enclosing transformer call
                [to-parsed? (if to-parsed-ok?
                                (expand-context-to-parsed? ctx)
                                #f)]
                [just-once? #f]
                [in-local-expand? #t]
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
