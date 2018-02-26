#lang racket/base
(require "../common/phase.rkt"
         (rename-in "syntax.rkt"
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
                    [identifier-binding-symbol raw:identifier-binding-symbol])
         (rename-in "track.rkt"
                    [syntax-track-origin raw:syntax-track-origin])
         "../expand/syntax-local.rkt"
         "srcloc.rkt"
         "../common/contract.rkt"
         (rename-in "debug.rkt"
                    [syntax-debug-info raw:syntax-debug-info])
         (only-in "../expand/context.rkt" get-current-expand-context)
         "../expand/log.rkt")

;; Provides public versions of syntax functions (with contract checks,
;; for example); see also "taint-api.rkt"

(provide syntax?
         syntax-e
         syntax-property
         syntax-property-preserved?
         syntax-property-symbol-keys
         syntax-original?
         syntax->datum
         maybe-syntax->datum
         datum->syntax
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
         identifier-prune-lexical-context
         syntax-shift-phase-level
         syntax-track-origin
         syntax-debug-info)

(define (syntax-e s)
  (check 'syntax-e syntax? s)
  (raw:syntax-e s))

(define (syntax->datum s)
  (check 'syntax->datum syntax? s)
  (raw:syntax->datum s))

(define (maybe-syntax->datum s)
  (if (syntax? s)
      (raw:syntax->datum s)
      s))

(define (datum->syntax stx-c s [stx-l #f] [stx-p #f] [ignored #f])
  (unless (or (not stx-c) (syntax? stx-c))
    (raise-argument-error 'datum->syntax "(or #f syntax?)" stx-c))
  (unless (or (not stx-l)
              (syntax? stx-l)
              (encoded-srcloc? stx-l))
    (raise-argument-error 'datum->syntax
                          (string-append "(or #f syntax?\n"
                                         "       (list/c any/c\n"
                                         "               (or/c exact-positive-integer? #f)\n"
                                         "               (or/c exact-nonnegative-integer? #f)\n"
                                         "               (or/c exact-positive-integer? #f)\n"
                                         "               (or/c exact-nonnegative-integer? #f))\n"
                                         "       (vector/c any/c\n"
                                         "                 (or/c exact-positive-integer? #f)\n"
                                         "                 (or/c exact-nonnegative-integer? #f)\n"
                                         "                 (or/c exact-positive-integer? #f)\n"
                                         "                 (or/c exact-nonnegative-integer? #f)))")
                          stx-l))
  (unless (or (not stx-p) (syntax? stx-p))
    (raise-argument-error 'datum->syntax "(or #f syntax?)" stx-p))
  (raw:datum->syntax stx-c s (to-srcloc-stx stx-l) stx-p))

(define (syntax->list s)
  (check 'syntax->list syntax? s)
  (raw:syntax->list s))

(define (syntax-original? s)
  (check 'syntax-original? syntax? s)
  (and (syntax-property s original-property-sym)
       (not (syntax-any-macro-scopes? s))))

(define (bound-identifier=? a b [phase (syntax-local-phase-level)])
  (check 'bound-identifier=? identifier? a)
  (check 'bound-identifier=? identifier? b)
  (unless (phase? phase)
    (raise-argument-error 'bound-identifier=? phase?-string phase))
  (raw:bound-identifier=? a b phase))

(define (free-identifier=? a b
                           [a-phase (syntax-local-phase-level)]
                           [b-phase a-phase])
  (check 'free-identifier=? identifier? a)
  (check 'free-identifier=? identifier? b)
  (unless (phase? a-phase)
    (raise-argument-error 'free-identifier=? phase?-string a-phase))
  (unless (phase? b-phase)
    (raise-argument-error 'free-identifier=? phase?-string b-phase))
  (raw:free-identifier=? a b a-phase b-phase))

(define (free-transformer-identifier=? a b)
  (check 'free-transformer-identifier=? identifier? a)
  (check 'free-transformer-identifier=? identifier? b)
  (define phase (add1 (syntax-local-phase-level)))
  (raw:free-identifier=? a b phase phase))

(define (free-template-identifier=? a b)
  (check 'free-template-identifier=? identifier? a)
  (check 'free-template-identifier=? identifier? b)
  (define phase (sub1 (syntax-local-phase-level)))
  (raw:free-identifier=? a b phase phase))

(define (free-label-identifier=? a b)
  (check 'free-label-identifier=? identifier? a)
  (check 'free-label-identifier=? identifier? b)
  (raw:free-identifier=? a b #f #f))

(define (identifier-binding id [phase  (syntax-local-phase-level)] [top-level-symbol? #f])
  (check 'identifier-binding identifier? id)
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding phase?-string phase))
  (raw:identifier-binding id phase top-level-symbol?))

(define (identifier-transformer-binding id [phase  (syntax-local-phase-level)])
  (check 'identifier-transformer-binding identifier? id)
  (raw:identifier-binding id (and phase (add1 phase))))

(define (identifier-template-binding id)
  (check 'identifier-template-binding identifier? id)
  (raw:identifier-binding id (sub1 (syntax-local-phase-level))))

(define (identifier-label-binding id)
  (check 'identifier-label-binding identifier? id)
  (raw:identifier-binding id #f))

(define (identifier-binding-symbol id [phase (syntax-local-phase-level)])
  (check 'identifier-binding-symbol identifier? id)
  (unless (phase? phase)
    (raise-argument-error 'identifier-binding-symbol phase?-string phase))
  (raw:identifier-binding-symbol id phase))

(define (identifier-prune-lexical-context id [syms null])
  (check 'identifier-prune-lexical-context identifier? id)
  (unless (and (list? syms)
               (andmap symbol? syms))
    (raise-argument-error 'identifier-prune-lexical-context "(listof symbol?)" syms))
  ;; It's a no-op in the Racket v6.5 expander
  id)

(define (syntax-debug-info s [phase (syntax-local-phase-level)] [all-bindings? #f])
  (check 'syntax-debug-info syntax? s)
  (unless (phase? phase)
    (raise-argument-error 'syntax-debug-info phase?-string phase))
  (raw:syntax-debug-info s phase all-bindings?))

(define (syntax-shift-phase-level s phase)
  (check 'syntax-shift-phase-level syntax? s)
  (unless (phase? phase)
    (raise-argument-error 'syntax-shift-phase-level phase?-string phase))
  (raw:syntax-shift-phase-level s phase))

(define (syntax-track-origin new-stx old-stx id)
  (check 'syntax-track-origin syntax? new-stx)
  (check 'syntax-track-origin syntax? old-stx)
  (check 'syntax-track-origin identifier? id)
  (define s (raw:syntax-track-origin new-stx old-stx id))
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (when ctx (log-expand ctx 'track-origin new-stx s))
  s)
