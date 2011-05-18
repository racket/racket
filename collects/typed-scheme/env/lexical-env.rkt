#lang scheme/base

;; this environment maps *lexical* variables to types
;; it also contains the proposition environment

;; these environments are unified in "Logical Types for Scheme"
;; but split here for performance

(require "../utils/utils.rkt"
	 "type-env-structs.rkt"
         "global-env.rkt"
	 unstable/mutated-vars syntax/id-table
         (only-in scheme/contract ->* -> or/c any/c listof cons/c)
         (utils tc-utils)
         (only-in (rep type-rep) Type/c)
         (typecheck tc-metafunctions)
	 (except-in (types utils convenience) -> ->*))

(provide lexical-env with-lexical-env with-lexical-env/extend
         with-lexical-env/extend/props)
(provide/cond-contract
 [lookup-type/lexical ((identifier?) (prop-env? #:fail (or/c #f (-> any/c #f))) . ->* . (or/c Type/c #f))]
 [update-type/lexical (((identifier? Type/c . -> . Type/c) identifier?) (prop-env?) . ->* . env?)])

;; the current lexical environment
(define lexical-env (make-parameter (make-empty-prop-env (make-immutable-free-id-table))))

;; run code in a new env
(define-syntax-rule (with-lexical-env e . b)
  (parameterize ([lexical-env e]) . b))

;; run code in an extended env
(define-syntax-rule (with-lexical-env/extend is ts . b)
  (with-lexical-env (extend/values is ts (lexical-env)) . b))

;; run code in an extended env and with replaced props
(define-syntax-rule (with-lexical-env/extend/props is ts ps . b)
  (with-lexical-env (replace-props (extend/values is ts (lexical-env)) ps) . b))

;; find the type of identifier i, looking first in the lexical env, then in the top-level env
;; identifer -> Type
(define (lookup-type/lexical i [env (lexical-env)] #:fail [fail #f])
  (lookup env i (λ (i) (lookup-type i (λ () ((or fail lookup-fail) i))))))

;; refine the type of i in the lexical env
;; (identifier type -> type) identifier -> environment
;; a macro for inlining :(
(define-syntax-rule (update-type/lexical f i env)
  ;; check if i is ever the target of a set!
  (if (is-var-mutated? i)
      ;; if it is, we do nothing
      env
      ;; otherwise, refine the type
      (parameterize
          ([current-orig-stx i])
        (let* ([v (lookup-type/lexical i env #:fail (lambda _ Univ))]
               [new-v (f i v)]
               [new-env (extend env i new-v)])
          new-env))))

