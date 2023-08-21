#lang racket/base
(require "../common/phase.rkt"
         "../common/phase+space.rkt"
         "../common/module-path.rkt"
         "../common/set.rkt"
         (rename-in "syntax.rkt"
                    [syntax-srcloc raw:syntax-srcloc]
                    [syntax->datum raw:syntax->datum]
                    [datum->syntax raw:datum->syntax])
         "property.rkt"
         "original.rkt"
         (rename-in "to-list.rkt"
                    [syntax->list raw:syntax->list])
         (rename-in "scope.rkt"
                    [syntax-e raw:syntax-e]
                    [bound-identifier=? raw:bound-identifier=?]
                    [syntax-shift-phase-level raw:syntax-shift-phase-level])
         (rename-in "binding.rkt"
                    [free-identifier=? raw:free-identifier=?]
                    [identifier-binding raw:identifier-binding]
                    [identifier-binding-symbol raw:identifier-binding-symbol]
                    [identifier-distinct-binding raw:identifier-distinct-binding])
         (rename-in "track.rkt"
                    [syntax-track-origin raw:syntax-track-origin])
         (rename-in "binding-set.rkt"
                    [syntax-binding-set raw:syntax-binding-set]
                    [syntax-binding-set-extend raw:syntax-binding-set-extend]
                    [syntax-binding-set->syntax raw:syntax-binding-set->syntax])
         "../expand/syntax-local.rkt"
         "srcloc.rkt"
         "../common/contract.rkt"
         (rename-in "debug.rkt"
                    [syntax-debug-info raw:syntax-debug-info])
         (only-in "../expand/context.rkt"
                  get-current-expand-context
                  expand-context-namespace)
         (only-in "../namespace/module.rkt"
                  namespace-module-get-portal-syntax-lookup)
         (only-in "../namespace/namespace.rkt"
                  current-namespace)
         "../expand/log.rkt"
         "mapped-name.rkt")

;; Provides public versions of syntax functions (with contract checks,
;; for example); see also "taint-api.rkt"

(provide syntax?
         syntax-e
         syntax-srcloc
         syntax-property
         syntax-property-remove
         syntax-property-preserved?
         syntax-property-symbol-keys
         syntax-original?
         syntax->datum
         maybe-syntax->datum
         datum->syntax
         syntax-binding-set?
         syntax-binding-set
         syntax-binding-set-extend
         syntax-binding-set->syntax
         syntax->list
         identifier?
         bound-identifier=?
         free-identifier=?
         free-transformer-identifier=?
         free-template-identifier=?
         free-label-identifier=?
         identifier-binding
         identifier-transformer-binding
         identifier-template-binding
         identifier-label-binding
         identifier-binding-symbol
         identifier-distinct-binding
         identifier-binding-portal-syntax
         identifier-prune-lexical-context
         syntax-shift-phase-level
         syntax-track-origin
         syntax-debug-info
         syntax-bound-symbols
         syntax-bound-phases)

(define/who (syntax-e s)
  (check who syntax? s)
  (raw:syntax-e s))

(define/who (syntax-srcloc s)
  (check who syntax? s)
  (raw:syntax-srcloc s))

(define/who (syntax->datum s)
  (check who syntax? s)
  (raw:syntax->datum s))

(define (maybe-syntax->datum s)
  (if (syntax? s)
      (raw:syntax->datum s)
      s))

(define/who (datum->syntax stx-c s [stx-l #f] [stx-p #f] [ignored #f])
  (unless (or (not stx-c) (syntax? stx-c))
    (raise-argument-error who "(or/c #f syntax?)" stx-c))
  (unless (or (not stx-l)
              (syntax? stx-l)
              (srcloc? stx-l)
              (encoded-srcloc? stx-l))
    (raise-argument-error who
                          (string-append "(or/c #f syntax?\n"
                                         "         srcloc?\n"
                                         "         (list/c any/c\n"
                                         "                 (or/c exact-positive-integer? #f)\n"
                                         "                 (or/c exact-nonnegative-integer? #f)\n"
                                         "                 (or/c exact-positive-integer? #f)\n"
                                         "                 (or/c exact-nonnegative-integer? #f))\n"
                                         "         (vector/c any/c\n"
                                         "                   (or/c exact-positive-integer? #f)\n"
                                         "                   (or/c exact-nonnegative-integer? #f)\n"
                                         "                   (or/c exact-positive-integer? #f)\n"
                                         "                   (or/c exact-nonnegative-integer? #f)))")
                          stx-l))
  (unless (or (not stx-p) (syntax? stx-p))
    (raise-argument-error who "(or/c #f syntax?)" stx-p))
  (raw:datum->syntax stx-c s (to-srcloc-stx stx-l) stx-p))

(define/who (syntax-binding-set)
  (raw:syntax-binding-set null))

(define/who (syntax-binding-set-extend bs as-sym as-phase mpi
                                       [sym as-sym]
                                       [phase as-phase]
                                       [nominal-mpi mpi]
                                       [nominal-phase+space phase]
                                       [nominal-sym sym]
                                       [nominal-require-phase+space-shift 0]
                                       [insp #f])
  (check who syntax-binding-set? bs)
  (check who symbol? as-sym)
  (check who phase? #:contract phase?-string as-phase)
  (check who module-path-index? mpi)
  (check who symbol? sym)
  (check who phase? #:contract phase?-string phase)
  (check who module-path-index? nominal-mpi)
  (check who phase+space? #:contract phase+space?-string nominal-phase+space)
  (check who symbol? nominal-sym)
  (check who phase+space-shift? #:contract phase+space-shift?-string nominal-require-phase+space-shift)
  (check who inspector? #:or-false insp)
  (raw:syntax-binding-set-extend bs as-sym as-phase mpi
                                 sym phase
                                 nominal-mpi (intern-phase+space nominal-phase+space) nominal-sym
                                 (intern-phase+space-shift nominal-require-phase+space-shift)
                                 insp))

(define/who (syntax-binding-set->syntax bs datum)
  (check who syntax-binding-set? bs)
  (raw:syntax-binding-set->syntax bs datum))

(define/who (syntax->list s)
  (check who syntax? s)
  (raw:syntax->list s))

(define/who (syntax-original? s)
  (check who syntax? s)
  (and (syntax-property s original-property-sym)
       (not (syntax-any-macro-scopes? s))))

(define/who (bound-identifier=? a b [phase (syntax-local-phase-level)])
  (check who identifier? a)
  (check who identifier? b)
  (check who phase? #:contract phase?-string phase)
  (raw:bound-identifier=? a b phase))

(define/who (free-identifier=? a b
                               [a-phase (syntax-local-phase-level)]
                               [b-phase a-phase])
  (check who identifier? a)
  (check who identifier? b)
  (check who phase? #:contract phase?-string a-phase)
  (check who phase? #:contract phase?-string b-phase)
  (raw:free-identifier=? a b a-phase b-phase))

(define/who (free-transformer-identifier=? a b)
  (check who identifier? a)
  (check who identifier? b)
  (define phase (add1 (syntax-local-phase-level)))
  (raw:free-identifier=? a b phase phase))

(define/who (free-template-identifier=? a b)
  (check who identifier? a)
  (check who identifier? b)
  (define phase (sub1 (syntax-local-phase-level)))
  (raw:free-identifier=? a b phase phase))

(define/who (free-label-identifier=? a b)
  (check who identifier? a)
  (check who identifier? b)
  (raw:free-identifier=? a b #f #f))

(define/who (identifier-binding id [phase (syntax-local-phase-level)] [top-level-symbol? #f] [exactly? #f])
  (check who identifier? id)
  (check who phase? #:contract phase?-string phase)
  (raw:identifier-binding id phase top-level-symbol? #:exactly? exactly?))

(define/who (identifier-transformer-binding id [phase  (syntax-local-phase-level)])
  (check who identifier? id)
  (raw:identifier-binding id (and phase (add1 phase))))

(define/who (identifier-template-binding id)
  (check who identifier? id)
  (raw:identifier-binding id (sub1 (syntax-local-phase-level))))

(define/who (identifier-label-binding id)
  (check who identifier? id)
  (raw:identifier-binding id #f))

(define/who (identifier-binding-symbol id [phase (syntax-local-phase-level)])
  (check who identifier? id)
  (check who phase? #:contract phase?-string phase)
  (raw:identifier-binding-symbol id phase))

(define/who (identifier-distinct-binding id other-id [phase (syntax-local-phase-level)] [top-level-symbol? #f])
  (check who identifier? id)
  (check who identifier? other-id)
  (check who phase? #:contract phase?-string phase)
  (raw:identifier-distinct-binding id other-id phase top-level-symbol?))

(define/who (identifier-binding-portal-syntax id [phase (syntax-local-phase-level)])
  (check who identifier? id)
  (check who phase? #:contract phase?-string phase)
  (define b (resolve+shift id phase #:unbound-sym? #t))
  (cond
    [(module-binding? b)
     (define ctx (get-current-expand-context #:fail-ok? #t))
     (define phase-shift (phase- phase (module-binding-phase b)))
     (define portal-syntax-lookup
       (namespace-module-get-portal-syntax-lookup (if ctx
                                                      (expand-context-namespace ctx)
                                                      (current-namespace))
                                                  (module-binding-module b)
                                                  phase-shift))
     (portal-syntax-lookup (module-binding-phase b) (module-binding-sym b))]
    [else #f]))

(define/who (identifier-prune-lexical-context id [syms null])
  (check who identifier? id)
  (unless (and (list? syms)
               (andmap symbol? syms))
    (raise-argument-error who "(listof symbol?)" syms))
  ;; It's a no-op in the Racket v6.5 expander
  id)

(define/who (syntax-debug-info s [phase (syntax-local-phase-level)] [all-bindings? #f])
  (check who syntax? s)
  (check who phase? #:contract phase?-string phase)
  (raw:syntax-debug-info s phase all-bindings?))

(define/who (syntax-shift-phase-level s phase)
  (check who syntax? s)
  (check who phase? #:contract phase?-string phase)
  (raw:syntax-shift-phase-level s phase))

(define/who (syntax-track-origin new-stx old-stx id)
  (check who syntax? new-stx)
  (check who syntax? old-stx)
  (check who identifier? id)
  (define s (raw:syntax-track-origin new-stx old-stx id))
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (when ctx (log-expand ctx 'track-syntax 'track-origin new-stx s))
  s)

(define/who (syntax-bound-symbols stx [phase (syntax-local-phase-level)] [exactly? #f])
  (check who syntax? stx)
  (set->list (syntax-mapped-names stx phase #:only-interned? #t #:exactly? exactly?)))

(define/who (syntax-bound-phases stx)
  (check who syntax? stx)
  (set->list (syntax-mapped-phases stx)))
