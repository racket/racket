#lang scheme/base
(require "test-utils.ss" (for-syntax scheme/base))
(require (private planet-requires type-comparison parse-type type-rep
                  type-effect-convenience tc-utils type-environments
                  type-name-env init-envs union))

(require (except-in (private base-env)))

(require (schemeunit))

(provide parse-type-tests)

(define-syntax (run-one stx)
  (syntax-case stx ()
    [(_ ty) #'(parameterize ([current-tvars initial-tvar-env]
                             [current-orig-stx #'here]
                             [orig-module-stx #'here]
                             [expanded-module-stx #'here])
                (parse-type (syntax ty)))]))

(define-syntax (pt-test stx)
  (syntax-case stx ()
    [(_ ts tv) #'(pt-test ts tv () initial-tvar-env)]
    [(_ ts tv tys) #'(pt-test ts tv tys initial-tvar-env)]
    [(_ ty-stx ty-val ((nm ty) ...) tvar-env)
     #`(test-case #,(format "~a" (syntax->datum #'ty-stx))
                  (parameterize ([current-tvars tvar-env])
                    #;(initialize-type-name-env initial-type-names)
                    (register-type-name #'nm ty) ...
                    (check type-equal? (parse-type (syntax ty-stx)) ty-val)))]))

(define-syntax pt-tests
  (syntax-rules ()
    [(_ nm [elems ...] ...)
     (test-suite nm
                 (pt-test elems ...) ...)]))

(define (parse-type-tests)  
  (pt-tests
   "parse-type tests" 
   [Number N]
   [Any Univ]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(list-of Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(pred Number) (make-pred-ty N)]
   [(values Number Boolean Number) (-values (list N B N))]
   [(Number -> Number) (-> N N)]
   [(Number -> Number) (-> N N)]
   [(Number Number Number Boolean -> Number) (N N N B . -> . N)]
   [(Number Number Number .. -> Boolean) ((list N N) N . ->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(Un Number Boolean) (Un N B)]
   [(Un Number Boolean Number) (Un N B)]
   [(Un Number Boolean 1) (Un N B)]
   [(All (a) (list-of a)) (-poly (a) (make-Listof  a))]
   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   
   [(poly-lst Number) (make-Listof  N) ((poly-lst (-poly (a) (make-Listof  a))))
                      #;(extend-env (list 'poly-lst) (list (-poly (a) (make-Listof  a))) initial-type-names)]
   
   [a (-v a) () (extend-env (list 'a) (list (-v a))
                            initial-tvar-env)]
   
   ))


(define (go)
  (run parse-type-tests))



