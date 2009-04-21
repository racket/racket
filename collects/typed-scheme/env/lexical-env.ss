#lang scheme/base

(require (except-in "../utils/utils.ss" extend))
(require "type-environments.ss" 
	 (utils tc-utils)
	 "type-env.ss"
	 (private mutated-vars)
	 (private type-utils)
	 (private type-effect-convenience))

(provide (all-defined-out))

;; the current lexical environment
(define lexical-env (make-parameter (make-empty-env free-identifier=?)))

;; run code in a new env
(define-syntax with-lexical-env
  (syntax-rules ()
    [(_ e . b) (parameterize ([lexical-env e]) . b)]))

;; run code in an extended env
(define-syntax with-lexical-env/extend
  (syntax-rules ()
    [(_ is ts . b) (parameterize ([lexical-env (extend/values is ts (lexical-env))]) . b)]))

;; find the type of identifier i, looking first in the lexical env, then in the top-level env
;; identifer -> Type
(define (lookup-type/lexical i [fail #f])
  (lookup (lexical-env) i 
          (lambda (i) (lookup-type 
                       i (lambda () 
                           (cond [(lookup (dotted-env) i (lambda _ #f))
                                  =>
                                  (lambda (a)
                                    (-lst (substitute Univ (cdr a) (car a))))]
                                 [else ((or fail lookup-fail) i)]))))))

;; refine the type of i in the lexical env
;; (identifier type -> type) identifier -> environment
(define (update-type/lexical f i)
  ;; do the updating on the given env
  ;; (identifier type -> type) identifier environment -> environment
  (define (update f k env)
    (parameterize
        ([current-orig-stx k])
      (let* ([v (lookup-type/lexical k (lambda _ Univ))]
             [new-v (f k v)]
             [new-env (extend env k new-v)])
        new-env)))
  ;; check if i is ever the target of a set!
  (if (is-var-mutated? i)
      ;; if it is, we do nothing
      (lexical-env)
      ;; otherwise, refine the type
      (update f i (lexical-env))))

;; convenience macro for typechecking in the context of an updated env
(define-syntax with-update-type/lexical
  (syntax-rules ()
    [(_ f i . b)
     (with-lexical-env (update-type/lexical f i) . b)]))

