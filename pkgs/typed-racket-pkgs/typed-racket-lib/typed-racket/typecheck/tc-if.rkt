#lang racket/unit
(require "../utils/utils.rkt"
         "signatures.rkt"
         (rep type-rep filter-rep object-rep)
         (types abbrev union utils filter-ops)
         (env lexical-env type-env-structs)
         (utils tc-utils)
         "tc-envops.rkt"
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
       (tc-expr/check expr Univ)
       (ret (Un))]
      [else (ret (Un))]))
  (match (single-value tst)
    [(tc-result1: _ (and f1 (FilterSet: fs+ fs-)) _)
     (define flag+ (box #t))
     (define flag- (box #t))
     (define results-t
       (with-lexical-env/extend-props (list fs+) #:flag flag+
         (tc thn (unbox flag+))))
     (define results-u
       (with-lexical-env/extend-props (list fs-) #:flag flag-
         (tc els (unbox flag-))))

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
     (match* (results-t results-u)
       [((tc-any-results: f1) (tc-any-results: f2))
        (tc-any-results (-or (-and fs+ f1) (-and fs- f2)))]
       ;; Not do awful things here
       [((tc-results: ts (list (FilterSet: f+ f-) ...) os) (tc-any-results: f2))
        (tc-any-results (-or (apply -and (map -or f+ f-)) f2))]
       [((tc-any-results: f2) (tc-results: ts (list (FilterSet: f+ f-) ...) os))
        (tc-any-results (-or (apply -and (map -or f+ f-)) f2))]
       [((tc-results: ts fs2 os2)
         (tc-results: us fs3 os3))
        ;; if we have the same number of values in both cases
        (cond [(= (length ts) (length us))
               (combine-results
                (for/list ([f2 (in-list fs2)] [f3 (in-list fs3)]
                           [t2 (in-list ts)] [t3 (in-list us)]
                           [o2 (in-list os2)] [o3 (in-list os3)])
                  (let ([filter
                         (match* (f2 f3)
                           [((FilterSet: f2+ f2-) (FilterSet: f3+ f3-))
                            (-FS (-or f2+ f3+) (-or f2- f3-))])]
                        [type (Un t2 t3)]
                        [object (if (object-equal? o2 o3) o2 -empty-obj)])
                    (ret type filter object))))]
              ;; special case if one of the branches is unreachable
              [(and (= 1 (length us)) (type-equal? (car us) (Un)))
               (ret ts fs2 os2)]
              [(and (= 1 (length ts)) (type-equal? (car ts) (Un)))
               (ret us fs3 os3)]
              ;; otherwise, error
           [else
            (tc-error/expr "Expected the same number of values from both branches of `if' expression, but got ~a and ~a"
                           (length ts) (length us))])])]))
