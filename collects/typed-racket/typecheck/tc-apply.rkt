#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-app-helper.rkt"
         racket/match racket/list
         (for-syntax (utils tc-utils))
         (private type-annotation)
         (types utils union subtype resolve abbrev type-table substitute)
         (utils tc-utils)
         (only-in srfi/1 alist-delete)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep object-rep rep-utils)
         (r:infer infer)
         '#%paramz
         (for-template
          racket/unsafe/ops
          (only-in '#%kernel [apply k:apply])
          "internal-forms.rkt" racket/base racket/bool '#%paramz
          (only-in racket/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-let^ tc-app^)
(export tc-apply^)

(define (do-ret t)
  (match t
    [(Values: (list (Result: ts _ _) ...)) (ret ts)]
    [(ValuesDots: (list (Result: ts _ _) ...) dty dbound) (ret ts (for/list ([t ts]) (-FS null null)) (for/list ([t ts]) (make-Empty)) dty dbound)]
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
    [(tc-result1: (and t (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ...))))
     ;; special case for (case-lambda)
     (when (null? doms)
       (tc-error/expr #:return (ret (Un))
                      "empty case-lambda given as argument to apply"))
     (match-let* ([arg-tres (map tc-expr fixed-args)]
                  [arg-tys (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                  [(tc-result1: tail-ty) (single-value tail)])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond
           ;; we've run out of cases to try, so error out
           [(null? doms*)
            (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty #f
                               #:return (ret (Un))
                               #:msg-thunk (lambda (dom)
                                             (string-append
                                              "Bad arguments to function in apply:\n"
                                              dom)))]
           ;; this case of the function type has a rest argument
           [(and (car rests*)
                 ;; check that the tail expression is a subtype of the rest argument
                 (subtype (apply -lst* arg-tys #:tail tail-ty)
                          (apply -lst* (car doms*) #:tail (make-Listof (car rests*)))))
            (do-ret (car rngs*))]
           ;; the function expects a dotted rest arg, so make sure we have a ListDots
           [(and (car drests*)
                 (match tail-ty
                   [(ListDots: tail-ty tail-bound)
                    ;; the check that it's the same bound
                    (and (eq? (cdr (car drests*)) tail-bound)
                         ;; and that the types are correct
                         (subtypes arg-tys (car doms*))
                         (subtype tail-ty (car (car drests*))))]
                   [_ #f]))
            (do-ret (car rngs*))]
           ;; the function has no rest argument, but provides all the necessary fixed arguments           
           [(and (not (car rests*)) (not (car drests*))
                 (subtype (apply -lst* arg-tys #:tail tail-ty)
                          (apply -lst* (car doms*))))
            (do-ret (car rngs*))]
           ;; otherwise, nothing worked, move on to the next case
           [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    ;; apply of simple polymorphic function
    [(tc-result1: (Poly: vars (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tres) (map tc-expr fixed-args)]
                   [(arg-tys) (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                   [(tail-ty tail-bound) (match (tc-expr/t tail)
                                           [(ListDots: tail-ty tail-bound)
                                            (values tail-ty tail-bound)]
                                           [t (values t #f)])])       
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty
                  [(tc-result1: (and t (Poly-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1)))))
                   (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty tail-bound
                                      #:return (ret (Un))
                                      #:msg-thunk (lambda (dom)
                                                    (string-append
                                                     "Bad arguments to polymorphic function in apply:\n"
                                                     dom)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars null
                                   (cons tail-ty arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; the function has no rest argument, but provides all the necessary fixed arguments           
               [(and (not (car rests*)) (not (car drests*)) (not tail-bound)
                     (infer vars null
                            (list (apply -lst* arg-tys #:tail tail-ty))
                            (list (apply -lst* (car doms*)))
                            (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars null
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg
               [(and (car drests*)
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))
                     (infer vars null (cons tail-ty arg-tys) (cons (car (car drests*)) (car doms*))
                            (car rngs*)))
                => (lambda (substitution) (do-ret (subst-all substitution (car rngs*))))]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result1: (Poly: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result1: (PolyDots: (and vars (list fixed-vars ... dotted-var))
                            (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1))))
     (let*-values ([(arg-tres) (map tc-expr fixed-args)]
                   [(arg-tys) (map (match-lambda [(tc-result1: t _ _) t]) arg-tres)]
                   [(tail-ty tail-bound) (match (tc-expr/t tail)
                                           [(ListDots: tail-ty tail-bound)
                                            (values tail-ty tail-bound)]
                                           [t (values t #f)])])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (define (finish substitution) (do-ret (subst-all substitution (car rngs*))))
         (cond [(null? doms*)
                (match f-ty
                  [(tc-result1: (and t (PolyDots-names: _ (Function: (list (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...)) ..1)))))
                   (domain-mismatches f args t doms rests drests rngs arg-tres tail-ty tail-bound
                                      #:return (ret (Un))
                                      #:msg-thunk (lambda (dom)
                                                    (string-append
                                                     "Bad arguments to polymorphic function in apply:\n"
                                                     dom)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg fixed-vars (list dotted-var)
                                   (cons tail-ty arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => finish]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg fixed-vars (list dotted-var)
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)))
                => finish]
               ;; ... function, ... arg, same bound on ...
               [(and (car drests*)
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))
                     (infer fixed-vars (list dotted-var)
                            (cons (make-ListDots tail-ty tail-bound) arg-tys)
                            (cons (make-ListDots (car (car drests*)) (cdr (car drests*))) (car doms*))
                            (car rngs*)))
                => finish]
               ;; ... function, ... arg, different bound on ...
               [(and (car drests*)
                     tail-bound
                     (not (eq? tail-bound (cdr (car drests*))))
                     (= (length (car doms*))
                        (length arg-tys))
                     (extend-tvars (list tail-bound (cdr (car drests*)))
                       (extend-indexes (cdr (car drests*))
                         ;; don't need to add tail-bound - it must already be an index
                         (infer fixed-vars (list dotted-var)
                                (cons (make-ListDots tail-ty tail-bound) arg-tys)
                                (cons (make-ListDots (car (car drests*)) (cdr (car drests*))) (car doms*))
                                (car rngs*)))))
                => finish]
               ;; ... function, (List A B C etc) arg
               [(and (car drests*)
                     (not tail-bound)
                     (eq? (cdr (car drests*)) dotted-var)
                     (= (length (car doms*))
                        (length arg-tys))
                     (untuple tail-ty)
                     (infer/dots fixed-vars dotted-var (append arg-tys (untuple tail-ty)) (car doms*)
                                 (car (car drests*)) (car rngs*) (fv (car rngs*))))
                => finish]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result1: (PolyDots: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result1: f-ty) (tc-error/expr #:return (ret (Un))
                         "Type of argument to apply is not a function type: \n~a" f-ty)]))
