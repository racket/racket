#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match racket/list
         (typecheck signatures tc-app-helper)
         (types utils abbrev substitute)
         (utils tc-utils)
         (rep type-rep)
         (r:infer infer))

(import tc-expr^ tc-lambda^ tc-let^ tc-app^)
(export tc-apply^)

(define (do-ret t)
  (match t
    [(Values: (list (Result: ts _ _) ...)) (ret ts)]
    [(ValuesDots: (list (Result: ts _ _) ...) dty dbound)
     (ret ts
          (for/list ([t (in-list ts)]) -top-filter)
          (for/list ([t (in-list ts)]) -empty-obj)
          dty dbound)]
    [_ (int-err "do-ret fails: ~a" t)]))

(define (tc/apply f args)
  (define f-ty (single-value f))
  ;; produces the first n-1 elements of the list, and the last element
  (define (split l) (let-values ([(f r) (split-at l (sub1 (length l)))])
                      (values f (car r))))
  (define-values (fixed-args tail)
    (let ([args* (syntax->list args)])
      (if (null? args*)
          (tc-error "apply requires a final list argument, given only a function argument of type ~a" (match f-ty [(tc-result1: t) t]))
          (split args*))))

  (define arg-tres (map tc-expr fixed-args))
  (define arg-tys (map (match-lambda [(tc-result1: t _ _) t]) arg-tres))
  (define full-tail-ty (tc-expr/t tail))
  (define-values (tail-ty tail-bound)
    (match full-tail-ty
      [(ListDots: tail-ty tail-bound)
       (values tail-ty tail-bound)]
      [t (values #f #f)]))

  ;; Raises an error message for the case that the arguments do not match any of the domains
  (define (failure)
    (match f-ty
      [(tc-result1:
         (and t (AnyPoly-names: _ _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1)))))
       (domain-mismatches f args t doms rests drests rngs arg-tres full-tail-ty #f
                          #:msg-thunk (lambda (dom)
                                        (string-append
                                         "Bad arguments to function in `apply':\n"
                                         dom)))]))

  (match f-ty
    ;; apply of a simple function or polymorphic function
    [(tc-result1: (AnyPoly: vars dotted-vars (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (or
       (for/or ([domain (in-list doms)]
                [range (in-list rngs)]
                [rest (in-list rests)]
                [drest (in-list drests)])
         ;; Takes a possible substitution and comuptes the substituted range type if it is not #f
         (define (finish substitution)
           (and substitution (do-ret (subst-all substitution range))))

         (finish
           (infer vars dotted-vars
                  (list (-Tuple* arg-tys full-tail-ty))
                  (list (-Tuple* domain
                                 (cond
                                   ;; the actual work, when we have a * function
                                   [rest (make-Listof rest)]
                                   ;; ... function
                                   [drest (make-ListDots (car drest) (cdr drest))]
                                   ;; the function has no rest argument,
                                   ;; but provides all the necessary fixed arguments
                                   [else -Null])))
                  range)))
       (failure))]
    [(tc-result1: (AnyPoly: _ _ (Function: '())))
     (tc-error/expr "Function has no cases")]
    [(tc-result1: f-ty)
     (tc-error/expr "Type of argument to apply is not a function type: \n~a" f-ty)]))
