#lang racket/base
(require "test-utils.rkt" (for-syntax racket/base)
         (utils tc-utils)
         (env type-alias-env type-env-structs tvar-env type-name-env init-envs)
         (rep type-rep)
         (rename-in (types subtype union utils abbrev numeric-tower)
                    [Un t:Un] [-> t:->] [->* t:->*])
         (base-env base-types base-types-extra colon)
         (submod typed-racket/base-env/base-types initialize)
         (for-template (base-env base-types base-types-extra base-env colon))
         (private parse-type)
         rackunit
         racket/dict)

(provide parse-type-tests)

;; HORRIBLE HACK!
;; We are solving the following problem:
;; when we require "base-env.rkt" for template, it constructs the type-alias-env
;; in phase 0 (relative to this module), but populates it with phase -1 identifiers
;; The identifiers are also bound in this module at phase -1, but the comparison for
;; the table is phase 0, so they don't compare correctly

;; The solution is to add the identifiers to the table at phase 0.
;; We do this by going through the table, constructing new identifiers based on the symbol
;; of the old identifier.
;; This relies on the identifiers being bound at phase 0 in this module (which they are,
;; because we have a phase 0 require of "base-env.rkt").
(initialize-type-names)
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
  (syntax-case stx (FAIL)
    [(_ FAIL ty-stx)
     (syntax/loc stx (pt-test FAIL ty-stx initial-tvar-env))]
    [(_ FAIL ty-stx tvar-env)
     (quasisyntax/loc stx
       (test-exn #,(format "~a" (syntax->datum #'ty-stx))
                 exn:fail:syntax?
                 (parameterize ([current-tvars tvar-env]
                                [delay-errors? #f])
                   (lambda () (parse-type (quote-syntax ty-stx))))))]
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
   [FAIL UNBOUND]
   [FAIL List]
   [FAIL (All (A) (List -> Boolean))]
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
   [(All (A) Number -> Number) (-poly (a) (t:-> N N))]
   [(All (A) (Number -> Number)) (-poly (a) (t:-> N N))]
   [(All (A) A -> A) (-poly (a) (t:-> a a))]
   [(All (A) A → A) (-poly (a) (t:-> a a))]
   [(All (A) (A -> A)) (-poly (a) (t:-> a a))]
   ;; requires transformer time stuff that doesn't work
   #;[(Refinement even?) (make-Refinement #'even?)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . t:->* . B)]
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
   [(case-> (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   ['(1 2 3) (-Tuple (map -val '(1 2 3)))]

   [(Listof Number) (make-Listof  N)]

   [a (-v a) (dict-set initial-tvar-env 'a (-v a))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]

   [(Any -> Boolean : Number) (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-FS (-not-filter -Number 0 null) (-filter -Number 0 null)))]
   [(Number -> Number -> Number)
    (t:-> -Number (t:-> -Number -Number))]
   [(Integer -> (All (X) (X -> X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]

   [(Opaque foo?) (make-Opaque #'foo?)]
   ;; PR 14122
   [FAIL (Opaque 3)]
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results

(define-go
  parse-type-tests)



