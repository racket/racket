#lang scheme/base
(require "test-utils.ss" (for-syntax scheme/base)
         (utils tc-utils)
	 (env type-alias-env type-environments type-name-env init-envs)
	 (rep type-rep)
	 (rename-in (types comparison subtype union utils convenience)
                    [Un t:Un] [-> t:->])
         (private base-types-new base-types-extra colon)
         (for-template (private base-types-new base-types-extra base-env colon))
         (private parse-type)
         racunit)

(provide parse-type-tests)

;; HORRIBLE HACK!
;; We are solving the following problem:
;; when we require "base-env.ss" for template, it constructs the type-alias-env
;; in phase 0 (relative to this module), but populates it with phase -1 identifiers
;; The identifiers are also bound in this module at phase -1, but the comparison for
;; the table is phase 0, so they don't compare correctly

;; The solution is to add the identifiers to the table at phase 0.
;; We do this by going through the table, constructing new identifiers based on the symbol
;; of the old identifier.
;; This relies on the identifiers being bound at phase 0 in this module (which they are, 
;; because we have a phase 0 require of "base-env.ss").
(for ([pr (type-alias-env-map cons)])
  (let ([nm (car pr)]
        [ty (cdr pr)])
    (register-resolved-type-alias (datum->syntax #'here (syntax->datum nm)) ty)))

(define-syntax (run-one stx)
  (syntax-case stx ()
    [(_ ty) (syntax/loc stx
              (parameterize ([current-tvars initial-tvar-env]
                             [current-orig-stx #'ty]
                             [orig-module-stx #'ty]
                             [expanded-module-stx #'ty]
                             [delay-errors? #f])
                (parse-type (syntax ty))))]))

(define-syntax (pt-test stx)
  (syntax-case stx ()
    [(_ ts tv) (syntax/loc stx (pt-test ts tv initial-tvar-env))]
    [(_ ty-stx ty-val tvar-env)
     (quasisyntax/loc
         stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
                  (parameterize ([current-tvars tvar-env]
                                 [delay-errors? #f])
                    (check type-equal? (parse-type (quote-syntax ty-stx)) ty-val))))]))

(define-syntax pt-tests
  (syntax-rules ()
    [(_ nm [elems ...] ...)
     (test-suite nm
                 (pt-test elems ...) ...)]))

(define N -Number)
(define B -Boolean)
(define Sym -Symbol)

(define (parse-type-tests)  
  (pt-tests
   "parse-type tests" 
   [Number N]
   [Any Univ]
   [(List Number String) (-Tuple (list N -String))]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(Listof Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(pred Number) (make-pred-ty N)]
   [(-> (values Number Boolean Number)) (t:-> (-values (list N B N)))]
   [(Number -> Number) (t:-> N N)]
   [(Number -> Number) (t:-> N N)]
   ;; requires transformer time stuff that doesn't work
   #;[(Refinement even?) (make-Refinement #'even?)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . ->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(U Number Boolean) (t:Un N B)]
   [(U Number Boolean Number) (t:Un N B)]
   [(U Number Boolean 1) (t:Un N B)]
   [(All (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(All (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(∀ (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> (values a ...)))
    (-polydots (a) (t:-> (make-ValuesDots (list) a 'a)))]
   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   ['(1 2 3) (-Tuple (map -val '(1 2 3)))]
   
   [(Listof Number) (make-Listof  N)]
   
   [a (-v a) (extend-env (list 'a) (list (-v a))
                            initial-tvar-env)]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   
   [(Any -> Boolean : Number) (make-pred-ty -Number)]
   
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results

(define-go
  parse-type-tests)



