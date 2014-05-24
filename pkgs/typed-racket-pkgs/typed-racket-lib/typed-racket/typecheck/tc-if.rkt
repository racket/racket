#lang racket/unit
(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep)
         (types abbrev union utils filter-ops)
         (env lexical-env type-env-structs)
         (utils tc-utils)
         (typecheck signatures tc-envops tc-metafunctions)
         (types type-table)
         racket/match)

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (define (tc expr reachable?)
    (unless reachable? (warn-unreachable expr))
    ;; If the code is unreachable, the resulting type should be Bottom.
    (if reachable?
        (tc-expr/check expr (and expected (erase-filter expected)))
        (ret -Bottom)))
  (match (single-value tst)
    [(tc-result1: _ (and f1 (FilterSet: fs+ fs-)) _)
     (define flag+ (box #t))
     (define flag- (box #t))
     (define results-t
       (with-lexical-env/extend-props (list fs+) #:flag flag+
         (add-unconditional-prop
           (tc thn (unbox flag+)) fs+)))
     (define results-u
       (with-lexical-env/extend-props (list fs-) #:flag flag-
         (add-unconditional-prop
           (tc els (unbox flag-)) fs-)))

     ;; record reachability
     (when (unbox flag+)
       (test-position-add-true tst))
     (when (unbox flag-)
       (test-position-add-false tst))
     (merge-tc-results (list results-t results-u))]))
