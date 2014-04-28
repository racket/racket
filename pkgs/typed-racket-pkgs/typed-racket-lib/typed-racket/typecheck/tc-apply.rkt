#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match racket/list
         (typecheck signatures tc-app-helper)
         (types utils union subtype abbrev substitute)
         (utils tc-utils)
         (env tvar-env index-env)
         (rep type-rep filter-rep)
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

  (match f-ty
    ;; apply of simple function
    [(tc-result1: (and t (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (match-let* ([arg-tres (map tc-expr fixed-args)]
                  [arg-tys (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                  [tail-ty (tc-expr/t tail)])
       (or
         (for/or ([domain (in-list doms)]
                  [range (in-list rngs)]
                  [rest (in-list rests)]
                  [drest (in-list drests)])
           (and
             (subtype
               (-Tuple* arg-tys tail-ty)
               (-Tuple* domain
                        (cond
                          ;; this case of the function type has a rest argument
                          [rest (make-Listof rest)]
                          ;; the function expects a dotted rest arg, so make sure we have a ListDots
                          [drest (make-ListDots (car drest) (cdr drest))]
                          ;; the function has no rest argument
                          [else (-val '())])))
             (do-ret range)))
         (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty #f
                            #:msg-thunk (lambda (dom)
                                          (string-append
                                           "Bad arguments to function in `apply':\n"
                                           dom)))))]
    ;; apply of simple polymorphic function
    [(tc-result1: (Poly: vars (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tres) (map tc-expr fixed-args)]
                   [(arg-tys) (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                   [(full-tail-ty) (tc-expr/t tail)]
                   [(tail-ty tail-bound)
                    (match full-tail-ty
                      [(ListDots: tail-ty tail-bound)
                       (values tail-ty tail-bound)]
                      [t (values #f #f)])])
       (or
         (for/or ([domain (in-list doms)]
                  [range (in-list rngs)]
                  [rest (in-list rests)]
                  [drest (in-list drests)])
           (define (finish substitution) (do-ret (subst-all substitution range)))
           (cond
             ;; the actual work, when we have a * function
             [(and rest
                   (infer vars null
                          (list (-Tuple* arg-tys full-tail-ty))
                          (list (-Tuple* domain (make-Listof rest)))
                          range))
              => finish]
             ;; the function has no rest argument, but provides all the necessary fixed arguments
             [(and (not rest) (not drest) (not tail-bound)
                   (infer vars null
                          (list (-Tuple* arg-tys full-tail-ty))
                          (list (-Tuple domain))
                          range))
              => finish]
             ;; ... function, ... arg
             [(and drest
                   tail-bound
                   (eq? tail-bound (cdr drest))
                   (= (length domain) (length arg-tys))
                   (infer vars null (cons tail-ty arg-tys) (cons (car drest) domain)
                          range))
              => finish]
             [else #f]))
         (match f-ty
           [(tc-result1: (and t (Poly-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1)))))
            (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty tail-bound
                               #:msg-thunk (lambda (dom)
                                             (string-append
                                              "Bad arguments to polymorphic function in `apply':\n"
                                                     dom)))])))]
    [(tc-result1: (PolyDots: (and vars (list fixed-vars ... dotted-var))
                            (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tres) (map tc-expr fixed-args)]
                   [(arg-tys) (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                   [(full-tail-ty) (tc-expr/t tail)]
                   [(tail-ty tail-bound)
                    (match full-tail-ty
                      [(ListDots: tail-ty tail-bound)
                       (values tail-ty tail-bound)]
                      [t (values #f #f)])])
       (or
         (for/or ([domain (in-list doms)]
                  [range (in-list rngs)]
                  [rest (in-list rests)]
                  [drest (in-list drests)])
           (define (finish substitution) (do-ret (subst-all substitution range)))
           (cond
             ;; the actual work, when we have a * function
             [(and rest
                   (infer fixed-vars (list dotted-var)
                          (list (-Tuple* arg-tys full-tail-ty))
                          (list (-Tuple* domain (make-Listof rest)))
                          range))
              => finish]
             ;; ... function, ... arg
             [(and drest tail-bound
                   (= (length domain) (length arg-tys))
                   (if (eq? tail-bound (cdr drest))
                       ;; same bound on the ...s
                       (infer fixed-vars (list dotted-var)
                              (cons (make-ListDots tail-ty tail-bound) arg-tys)
                              (cons (make-ListDots (car drest) (cdr drest)) domain)
                              range)
                       ;; different bounds on the ...s
                       (extend-tvars (list tail-bound (cdr drest))
                         (extend-indexes (cdr drest)
                           ;; don't need to add tail-bound - it must already be an index
                          (infer fixed-vars (list dotted-var)
                                 (cons (make-ListDots tail-ty tail-bound) arg-tys)
                                 (cons (make-ListDots (car drest) (cdr drest)) domain)
                                 range)))))
              => finish]
             ;; ... function, (Listof A) or (List A B C etc) arg
             [(and drest (not tail-bound)
                   (eq? (cdr drest) dotted-var)
                   (<= (length domain) (length arg-tys))
                   (match full-tail-ty
                     [(List: tail-arg-tys #:tail (Listof: tail-arg-ty))
                      (infer/vararg
                        fixed-vars (list dotted-var)
                        (cons tail-arg-ty (append arg-tys tail-arg-tys))
                        (cons (car drest) domain)
                        (car drest)
                        range)]
                     [(List: tail-arg-tys)
                      (infer/dots fixed-vars dotted-var (append arg-tys tail-arg-tys) domain
                                  (car drest) range (fv range))]
                     [_ #f]))
              => finish]
             [else #f]))
         ;; Nothing matched
         (match f-ty
           [(tc-result1: (and t (PolyDots-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1)))))
            (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty tail-bound
                               #:msg-thunk (lambda (dom)
                                             (string-append
                                              "Bad arguments to polymorphic function in `apply':\n"
                                              dom)))])))]
    [(tc-result1: (or (Function: '()) (Poly: _ (Function: '())) (PolyDots: _ (Function: '()))))
     (tc-error/expr "Function has no cases")]
    [(tc-result1: f-ty)
     (tc-error/expr "Type of argument to apply is not a function type: \n~a" f-ty)]))
