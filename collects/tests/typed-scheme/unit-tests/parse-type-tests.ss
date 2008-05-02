#lang scheme/base
(require "test-utils.ss" (for-syntax scheme/base))
(require (private planet-requires type-comparison parse-type type-rep
                  tc-utils type-environments type-alias-env
                  type-name-env init-envs union type-utils))

(require (rename-in (private type-effect-convenience) [-> t:->])
         (except-in (private base-types) Un)
         (for-template (private base-types)))

(require (schemeunit))

(provide parse-type-tests)

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

(define (parse-type-tests)  
  (pt-tests
   "parse-type tests" 
   [Number N]
   [Any Univ]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(Listof Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(pred Number) (make-pred-ty N)]
   [(values Number Boolean Number) (-values (list N B N))]
   [(Number -> Number) (t:-> N N)]
   [(Number -> Number) (t:-> N N)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
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
   
   [(Listof Number) (make-Listof  N)]
   
   [a (-v a) (extend-env (list 'a) (list (-v a))
                            initial-tvar-env)]
   
   ))


(define-go
  parse-type-tests)



