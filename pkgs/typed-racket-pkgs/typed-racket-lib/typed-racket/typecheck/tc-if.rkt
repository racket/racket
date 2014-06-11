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
    (cond
      ;; if reachable? is #f, then we don't want to verify that this branch has the appropriate type
      ;; in particular, it might be (void)
      [(and expected reachable?)
       (tc-expr/check expr (erase-filter expected))]
      ;; this code is reachable, but we have no expected type
      [reachable? (tc-expr expr)]
      ;; otherwise, this code is unreachable
      ;; and the resulting type should be the empty type
      [(check-unreachable-code?)
       (tc-expr expr)
       (ret (Un))]
      [else (ret (Un))]))
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
     ;; since we may typecheck a given piece of code multiple times in different
     ;; contexts, we need to take previous results into account
     (cond [(and (not (unbox flag+)) ; maybe contradiction
                 ;; to be an actual contradiction, we must have either previously
                 ;; recorded this test as a contradiction, or have never seen it
                 ;; before
                 (not (tautology? tst))
                 (not (neither? tst)))
            (add-contradiction tst)]
           [(and (not (unbox flag-)) ; maybe tautology
                 ;; mirror case
                 (not (contradiction? tst))
                 (not (neither? tst)))
            (add-tautology tst)]
           [else
            (add-neither tst)])
     (merge-tc-results (list results-t results-u))]))
