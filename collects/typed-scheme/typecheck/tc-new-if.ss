#lang scheme/unit


(require (rename-in "../utils/utils.ss" [infer r:infer]))
(require "signatures.ss"
         (rep type-rep filter-rep object-rep)
         (rename-in (types convenience subtype union utils comparison remove-intersect)
                    [remove *remove])
         (env lexical-env)
         (r:infer infer)
	 (utils tc-utils mutated-vars)
         (typecheck tc-envops tc-metafunctions)
         syntax/kerncase
         mzlib/trace
         mzlib/plt-match)

;; if typechecking   
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (define (tc e) (if expected (tc-expr/check e expected) (tc-expr e)))
  (match (tc-expr tst)
    [(list (tc-result: _ (and f1 (FilterSet: fs+ fs-)) _))
     (match-let ([(tc-results: ts fs2 _) (with-lexical-env (env+ (lexical-env) fs+) (tc thn))]
                 [(tc-results: us fs3 _) (with-lexical-env (env+ (lexical-env) fs-) (tc els))])
       ;; if we have the same number of values in both cases
       (cond [(= (length ts) (length us))
              (for/list ([t ts] [u us] [f2 fs2] [f3 fs3])
                (ret (Un t u) (combine-filter f1 f2 f2)))]
             [else
              (tc-error/expr #:ret (ret Err)
                             "Expected the same number of values from both branches of if expression, but got ~a and ~a"
                             (length ts) (length us))]))]
    [(tc-results: t _ _)
     (tc-error/expr #:ret (ret (or expected Err))
                    "Test expression expects one value, given ~a" t)]))

(define tc/if-twoarm/check tc/if-twoarm)