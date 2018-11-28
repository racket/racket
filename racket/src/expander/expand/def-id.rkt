#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/module-binding.rkt"
         "require+provide.rkt"
         "../namespace/namespace.rkt"
         "context.rkt"
         "root-expand-context.rkt"
         "env.rkt")

(provide select-defined-syms-and-bind!
         select-defined-syms-and-bind!/ctx
         add-defined-sym!)

;; For each identifier that is defined in a module or at the top
;; level, we need to map the identifier to a symbol for a variable in
;; a linklet instance. (Since multiple definitions have identifiers
;; that wrap the same symbol in different scopes, we invent new
;; symbols as unreadable symbols.) A `module-binding` refers to this
;; linklet-level symbol.

;; As a concession to top-level evaluation, reserve plain symbols for
;; identifers that have only the module's scopes. That way, if a
;; reference to an identifier is encountered before a definition, the
;; reference can still work in normal cases.

;; One further twist is that top-level expansion uses a "top level
;; bind scope", which is used to create bindings while expanding so
;; that definitions and uses expanded to together work in the expected
;; way, but no binding is actually created until a definition is
;; evaluated. For the purposes of selecting a symbol, we need to treat
;; as equivalent identifiers with and without the top level bind
;; scope.

(define (select-defined-syms-and-bind! ids defined-syms
                                       self phase all-scopes-stx
                                       #:frame-id frame-id
                                       #:top-level-bind-scope [top-level-bind-scope #f]
                                       #:requires+provides [requires+provides #f]
                                       #:in [orig-s #f]
                                       #:as-transformer? [as-transformer? #f])
  (define defined-syms-at-phase
    (or (hash-ref defined-syms phase #f) (let ([ht (make-hasheq)])
                                           (hash-set! defined-syms phase ht)
                                           ht)))
  (for/list ([id (in-list ids)])
    (define sym (syntax-e id))
    (define defined-sym
      (if (and (not (defined-as-other? (hash-ref defined-syms-at-phase sym #f) id phase top-level-bind-scope))
               ;; Only use `sym` directly if there are no
               ;; extra scopes on the binding form...
               (no-extra-scopes? id all-scopes-stx top-level-bind-scope phase)
               ;; ... and if it's interned
               (symbol-interned? sym))
          sym
          (let loop ([pos 1])
            (define s (string->unreadable-symbol (string-append (symbol->string sym) "." (number->string pos))))
            (if (defined-as-other? (hash-ref defined-syms-at-phase s #f) id phase top-level-bind-scope)
                (loop (add1 pos))
                s))))
    (hash-set! defined-syms-at-phase defined-sym id)
    (define b (make-module-binding self phase defined-sym #:frame-id frame-id
                                   #:nominal-sym sym))
    (when requires+provides
      (remove-required-id! requires+provides id phase #:unless-matches b))
    (add-binding! id b phase #:in orig-s)
    (when requires+provides
      (add-defined-or-required-id! requires+provides id phase b #:as-transformer? as-transformer?))
    defined-sym))

(define (no-extra-scopes? id all-scopes-stx top-level-bind-scope phase)
  (define m-id (datum->syntax all-scopes-stx (syntax-e id)))
  (or (bound-identifier=? id m-id phase)
      (and top-level-bind-scope
           (bound-identifier=? id (add-scope m-id top-level-bind-scope) phase))))

(define (defined-as-other? prev-id id phase top-level-bind-scope)
  (and prev-id
       (not (bound-identifier=? prev-id id phase))
       (or (not top-level-bind-scope)
           (not (bound-identifier=? (remove-scope prev-id top-level-bind-scope)
                                    (remove-scope id top-level-bind-scope)
                                    phase)))))

;; ------------------------------

(define (select-defined-syms-and-bind!/ctx tl-ids ctx)
  (select-defined-syms-and-bind! tl-ids (root-expand-context-defined-syms ctx)
                                 (root-expand-context-self-mpi ctx)
                                 (expand-context-phase ctx)
                                 (root-expand-context-all-scopes-stx ctx)
                                 #:frame-id (root-expand-context-frame-id ctx)
                                 #:top-level-bind-scope (root-expand-context-top-level-bind-scope ctx)))

;; ----------------------------------------

(define (add-defined-sym! defined-syms phase sym id)
  (define defined-syms-at-phase
    (or (hash-ref defined-syms phase #f) (let ([ht (make-hasheq)])
                                           (hash-set! defined-syms phase ht)
                                           ht)))
  (hash-set! defined-syms-at-phase sym id))
