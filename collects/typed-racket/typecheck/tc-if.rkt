#lang racket/unit
(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "check-below.rkt"
         (rep type-rep filter-rep object-rep)
         (types abbrev subtype union utils filter-ops)
         (env lexical-env type-env-structs)
         (r:infer infer)
         (utils tc-utils)
         "tc-envops.rkt" "tc-metafunctions.rkt"
         (types type-table)
         syntax/kerncase
         racket/match)

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (erase-filter tc)
  (match tc
    [(tc-results: ts _ _)
     (ret ts (for/list ([f ts]) (make-NoFilter)) (for/list ([f ts]) (make-NoObject)))]))

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
     (let*-values ([(flag+ flag-) (values (box #t) (box #t))])
       (match-let* ([env-thn (env+ (lexical-env) (list fs+) flag+)]
                    [env-els (env+ (lexical-env) (list fs-) flag-)]
                    [new-thn-props (filter (λ (e) (and (atomic-filter? e) (not (memq e (env-props (lexical-env))))))
                                           (env-props env-thn))]
                    [new-els-props (filter (λ (e) (and (atomic-filter? e) (not (memq e (env-props (lexical-env))))))
                                           (env-props env-els))]
                    [(tc-results: ts fs2 os2) (with-lexical-env env-thn (tc thn (unbox flag+)))]
                    [(tc-results: us fs3 os3) (with-lexical-env env-els (tc els (unbox flag-)))])
         ;(printf "old props: ~a\n" (env-props (lexical-env)))
         ;(printf "fs+: ~a\n" fs+)
         ;(printf "fs-: ~a\n" fs-)
         ;(printf "thn-props: ~a\n" (env-props env-thn))
         ;(printf "els-props: ~a\n" (env-props env-els))
         ;(printf "new-thn-props: ~a\n" new-thn-props)
         ;(printf "new-els-props: ~a\n" new-els-props)

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

         ;; if we have the same number of values in both cases
         (cond [(= (length ts) (length us))
                (let ([r (combine-results
                          (for/list ([f2 fs2] [f3 fs3] [t2 ts] [t3 us] [o2 os2] [o3 os3])
                            (let ([filter
                                   (match* (f2 f3)
                                     [((NoFilter:) _)
                                      (-FS -top -top)]
                                     [(_ (NoFilter:))
                                      (-FS -top -top)]
                                     [((FilterSet: f2+ f2-) (FilterSet: f3+ f3-))
                                      ;(printf "f2- ~a f+ ~a\n" f2- fs+)
                                      (-FS (-or (apply -and fs+ f2+ new-thn-props) (apply -and fs- f3+ new-els-props))
                                           (-or (apply -and fs+ f2- new-thn-props) (apply -and fs- f3- new-els-props)))])]
                                  [type (Un t2 t3)]
                                  [object (if (object-equal? o2 o3) o2 (make-Empty))])
                              ;(printf "result filter is: ~a\n" filter)
                              (ret type filter object))))])
                  (cond-check-below r expected))]
               ;; special case if one of the branches is unreachable
               [(and (= 1 (length us)) (type-equal? (car us) (Un)))
                (cond-check-below (ret ts fs2 os2) expected)]
               [(and (= 1 (length ts)) (type-equal? (car ts) (Un)))
                (cond-check-below (ret us fs3 os3) expected)]
               ;; otherwise, error
               [else
                (tc-error/expr #:return (ret (or expected Err))
                               "Expected the same number of values from both branches of `if' expression, but got ~a and ~a"
                               (length ts) (length us))])))]
    [(tc-results: t _ _)
     (tc-error/expr #:return (ret (or expected Err))
                    "Test expression expects one value, given ~a" t)]))
