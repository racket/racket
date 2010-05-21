#lang scheme/base

(require "../utils/utils.rkt"
	 "type-environments.rkt" 
	 "type-env.rkt"
	 unstable/mutated-vars syntax/id-table
         (only-in scheme/contract ->* -> or/c any/c listof cons/c)
         (utils tc-utils)
         (only-in (rep type-rep) Type/c)
         (typecheck tc-metafunctions)
	 (except-in (types utils convenience) -> ->*))

(provide lexical-env with-lexical-env with-lexical-env/extend with-update-type/lexical
         with-lexical-env/extend/props)
(p/c
 [lookup-type/lexical ((identifier?) (env? #:fail (or/c #f (-> any/c #f))) . ->* . (or/c Type/c #f))]
 [update-type/lexical (((identifier? Type/c . -> . Type/c) identifier?) (env?) . ->* . env?)])

;; the current lexical environment
(define lexical-env (make-parameter (make-empty-env (make-immutable-free-id-table))))

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
  (lookup env i 
          (lambda (i) (lookup-type 
                       i (lambda () 
                           (cond [(lookup (dotted-env) i (lambda _ #f))
                                  =>
                                  (lambda (a)
                                    (-lst (substitute Univ (cdr a) (car a))))]
                                 [else ((or fail lookup-fail) i)]))))))

;; refine the type of i in the lexical env
;; (identifier type -> type) identifier -> environment
(define (update-type/lexical f i [env (lexical-env)])
  ;; do the updating on the given env
  ;; (identifier type -> type) identifier environment -> environment
  (define (update f k env)
    (parameterize
        ([current-orig-stx k])
      (let* ([v (lookup-type/lexical k env #:fail (lambda _ Univ))]
             [new-v (f k v)]
             [new-env (extend env k new-v)])
        new-env)))
  ;; check if i is ever the target of a set!
  (if (is-var-mutated? i)
      ;; if it is, we do nothing
      env
      ;; otherwise, refine the type
      (update f i env)))

;; convenience macro for typechecking in the context of an updated env
(define-syntax with-update-type/lexical
  (syntax-rules ()
    [(_ f i . b)
     (with-lexical-env (update-type/lexical f i) . b)]))

