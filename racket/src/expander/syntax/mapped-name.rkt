#lang racket/base
(require "../common/set.rkt"
         "../common/scope-set.rkt"
         "../common/phase.rkt"
         "syntax.rkt"
         "scope.rkt"
         (submod "scope.rkt" for-debug)
         "binding-table.rkt"
         "fallback.rkt")

(provide syntax-mapped-names
         syntax-mapped-phases
         syntax-mapped-interned-scope-symbols)

(define (syntax-mapped-names s phase
                             #:only-interned? [only-interned? #f]
                             #:exactly? [exactly? #f])
  (define s-scs (syntax-scope-set s phase))
  (define-values (pl-reps normalize) (phaseless-info s))
  (define (add syms sc)
    (set-union syms
               (binding-table-symbols (scope-binding-table sc) s-scs s null
                                      #:only-interned? only-interned?
                                      #:exactly? exactly?
                                      #:normalize normalize)))
  (for/fold ([syms (for/fold ([syms (seteq)]) ([sc (in-scope-set s-scs)])
                     (add syms sc))])
            ([sc (in-list pl-reps)])
    (add syms sc)))

;; For an identifier with a phaseless multi-scope, extra binding tables
;; to consult and a normalizer for scope sets in those tables
(define (phaseless-info s)
  (define pl-reps (phaseless-representatives (fallback-first (syntax-shifted-multi-scopes s))))
  (values pl-reps
          (if (null? pl-reps)
              values
              (lambda (scs) (phaseless-normalize-scopes scs pl-reps)))))

(define (syntax-mapped-phases s)
  (define smss (fallback-first (syntax-shifted-multi-scopes s)))
  (for/fold ([phases (seteqv)]) ([sms (in-set smss)])
    (shifted-multi-scope-add-binding-phases sms phases)))

(define (syntax-mapped-interned-scope-symbols s phase #:exactly? exactly?)
  (define s-scs (syntax-scope-set s phase))
  (define sym (syntax-e s))
  (define-values (pl-reps normalize) (phaseless-info s))
  (define (add syms sc)
    (set-union syms
               (binding-table-interned-scope-keys (scope-binding-table sc) s-scs sym s null
                                                  #:exactly? exactly?
                                                  #:normalize normalize
                                                  interned-scope? interned-scope-key)))
  (for/fold ([syms (for/fold ([syms (seteq)]) ([sc (in-scope-set s-scs)])
                     (add syms sc))])
            ([sc (in-list pl-reps)])
    (add syms sc)))
