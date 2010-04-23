#lang scheme/unit


(require (rename-in "../utils/utils.ss" [infer r:infer]))
(require "signatures.ss"
         (rep type-rep filter-rep object-rep)
         (rename-in (types convenience subtype union utils comparison remove-intersect abbrev)
                    [remove *remove])
         (env lexical-env type-environments)
         (r:infer infer)
	 (utils tc-utils)
         (typecheck tc-envops tc-metafunctions)
         syntax/kerncase
         mzlib/trace unstable/debug
         scheme/match)

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
                    [new-thn-props (env-props env-thn)]
                    [new-els-props (env-props env-els)]
                    [(tc-results: ts fs2 os2) (with-lexical-env env-thn (tc thn (unbox flag+)))]
                    [(tc-results: us fs3 os3) (with-lexical-env env-els (tc els (unbox flag-)))])
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
                                      (-FS (-or (-and fs+ f2+) (-and fs- f3+))
                                           (-or (-and fs+ f2-) (-and fs- f3-)))])]
                                  [type (Un t2 t3)]
                                  [object (if (object-equal? o2 o3) o2 (make-Empty))])
                              ;(printf "result filter is: ~a\n" filter)
                              (debug ret type filter object))))])
                  (if expected (check-below r expected) r))]
               ;; special case if one of the branches is unreachable
               [(and (= 1 (length us)) (type-equal? (car us) (Un)))
                (if expected (check-below (ret ts fs2 os2) expected) (ret ts fs2 os2))]
               [(and (= 1 (length ts)) (type-equal? (car ts) (Un)))
                (if expected (check-below (ret us fs3 os3) expected) (ret us fs3 os3))]
               ;; otherwise, error
               [else
                (tc-error/expr #:return (ret (or expected Err))
                               "Expected the same number of values from both branches of `if' expression, but got ~a and ~a"
                               (length ts) (length us))])))]
    [(tc-results: t _ _)
     (tc-error/expr #:return (ret (or expected Err))
                    "Test expression expects one value, given ~a" t)]))
