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
    [(tc-result1: _ (and f1 (FilterSet: fs+ fs-)) _)
     (match-let ([(tc-results: ts fs2 _) (with-lexical-env (env+ (lexical-env) fs+) (tc thn))]
                 [(tc-results: us fs3 _) (with-lexical-env (env+ (lexical-env) fs-) (tc els))])
       ;; if we have the same number of values in both cases
       (cond [(= (length ts) (length us))
              (ret (for/list ([t ts] [u us]) (Un t u))
                   (for/list ([f2 fs2] [f3 fs3])
                             (combine-filter f1 f2 f3)))]
             [else
              (tc-error/expr #:return (ret Err)
                             "Expected the same number of values from both branches of if expression, but got ~a and ~a"
                             (length ts) (length us))]))]
    [(tc-results: t _ _)
     (tc-error/expr #:return (ret (or expected Err))
                    "Test expression expects one value, given ~a" t)]))

(define tc/if-twoarm/check tc/if-twoarm)