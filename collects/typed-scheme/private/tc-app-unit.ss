#lang scheme/unit

(require "signatures.ss"
         "type-rep.ss"
         "effect-rep.ss"
         "tc-utils.ss"
         "subtype.ss"
         "infer.ss"
         (only-in "utils.ss" debug in-syntax printf/log in-pairs)
         "union.ss"
         "type-utils.ss"
         "type-effect-convenience.ss"
         "type-effect-printer.ss"
         "type-annotation.ss"
         "resolve-type.ss"
         "type-environments.ss"
         (only-in srfi/1 alist-delete)
         (only-in scheme/private/class-internal make-object do-make-object)
         mzlib/trace mzlib/pretty syntax/kerncase scheme/match
         (for-syntax scheme/base)
         (for-template 
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))
(require "constraint-structs.ss")

(import tc-expr^ tc-lambda^ tc-dots^)
(export tc-app^)

;; comparators that inform the type system
(define (comparator? i)
  (or (free-identifier=? i #'eq?)
      (free-identifier=? i #'equal?)
      (free-identifier=? i #'eqv?)
      (free-identifier=? i #'=)
      (free-identifier=? i #'string=?)))

;; typecheck eq? applications
;; identifier identifier expression expression expression
;; identifier expr expr expr expr -> tc-result
(define (tc/eq comparator v1 v2)
  (define (e? i) (free-identifier=? i comparator))
  (define (do id val)
    (define-syntax alt (syntax-rules () [(_ nm pred ...) 
                                         (and (e? #'nm) (or (pred val) ...))]))
    (if (or (alt symbol=? symbol?)
            (alt string=? string?)
            (alt = number?)
            (alt eq? boolean? keyword? symbol?)
            (alt eqv? boolean? keyword? symbol? number?)
            (alt equal? (lambda (x) #t)))
        (values (list (make-Restrict-Effect (-val val) id))
                (list (make-Remove-Effect (-val val) id)))          
        (values (list) (list))))
  (match (list (tc-expr v1) (tc-expr v2))
    [(list (tc-result: id-t (list (Var-True-Effect: id1)) (list (Var-False-Effect: id2))) (tc-result: (Value: val)))
     (do id1 val)]
    [(list (tc-result: (Value: val)) (tc-result: id-t (list (Var-True-Effect: id1)) (list (Var-False-Effect: id2))))
     (do id1 val)]
    [_ (values (list) (list))]))


;; typecheck an application:
;; arg-types: the types of the actual parameters
;; arg-effs: the effects of the arguments
;; dom-types: the types of the function's fixed arguments
;; rest-type: the type of the functions's rest parameter, or #f
;; latent-eff: the latent effect of the function
;; arg-stxs: the syntax for each actual parameter, for error reporting
;; [Type] [Type] Maybe[Type] [Syntax] -> (values Listof[Effect] Listof[Effect])
(define (tc-args arg-types arg-thn-effs arg-els-effs dom-types rest-type latent-thn-eff latent-els-eff arg-stxs)
  (define (var-true-effect-v e) (match e
                                  [(Var-True-Effect: v) v]))
  (define (var-false-effect-v e) (match e
                                   [(Var-False-Effect: v) v]))
  ;; special case for predicates:
  (if (and (not (null? latent-thn-eff))
           (not (null? latent-els-eff))
           (not rest-type)
           ;(printf "got to =~n")
           (= (length arg-types) (length dom-types) 1)             
           ;(printf "got to var preds~n")
           (= (length (car arg-thn-effs)) (length (car arg-els-effs)) 1)
           (Var-True-Effect? (caar arg-thn-effs)) ;; thn-effs is a list for each arg
           (Var-False-Effect? (caar arg-els-effs)) ;; same with els-effs
           (free-identifier=? (var-true-effect-v (caar arg-thn-effs))
                              (var-false-effect-v (caar arg-els-effs)))
           (subtype (car arg-types) (car dom-types)))
      ;; then this was a predicate application, so we construct the appropriate type effect
      (values (map (add-var (var-true-effect-v (caar arg-thn-effs))) latent-thn-eff)
              (map (add-var (var-true-effect-v (caar arg-thn-effs))) latent-els-eff))
      ;; otherwise, we just ignore the effects.
      (let loop ([args arg-types] [doms dom-types] [stxs arg-stxs] [arg-count 1])
        (cond 
          [(and (null? args) (null? doms)) (values null null)] ;; here, we just return the empty effect
          [(null? args) 
           (tc-error/delayed
            "Insufficient arguments to function application, expected ~a, got ~a" 
            (length dom-types) (length arg-types))
           (values null null)]
          [(and (null? doms) rest-type)
           (if (subtype (car args) rest-type)
               (loop (cdr args) doms (cdr stxs) (add1 arg-count))
               (begin
                 (tc-error/delayed #:stx (car stxs)
                                   "Rest argument had wrong type, expected: ~a and got: ~a"
                                   rest-type (car args))
                 (values null null)))]
          [(null? doms)
           (tc-error/delayed "Too many arguments to function, expected ~a, got ~a" (length dom-types) (length arg-types))
           (values null null)]
          [(subtype (car args) (car doms))
           (loop (cdr args) (cdr doms) (cdr stxs) (add1 arg-count))]
          [else
           (tc-error/delayed 
            #:stx (car stxs)
            "Wrong function argument type, expected ~a, got ~a for argument ~a" 
            (car doms) (car args) arg-count)
           (loop (cdr args) (cdr doms) (cdr stxs) (add1 arg-count))]))))


;(trace tc-args)

(define (stringify-domain dom rst drst)
  (let ([doms-string (if (null? dom) "" (string-append (stringify dom) " "))])
    (cond [drst
           (format "~a~a ... ~a" doms-string (car drst) (cdr drst))]
          [rst
           (format "~a~a *" doms-string rst)]
          [else (stringify dom)])))

(define (domain-mismatches ty doms rests drests arg-tys tail-ty tail-bound)
  (cond
    [(null? doms)
     (int-err "How could doms be null: ~a ~a" ty)]
    [(= 1 (length doms))
     (format "Domain: ~a~nArguments: ~a~n"
             (stringify-domain (car doms) (car rests) (car drests))
             (stringify-domain arg-tys (if (not tail-bound) tail-ty #f) (if tail-bound (cons tail-ty tail-bound) #f)))]
    [else
     (format "Domains: ~a~nArguments: ~a~n"
             (stringify (map stringify-domain doms rests drests) "~n\t")
             (stringify-domain arg-tys (if (not tail-bound) tail-ty #f) (if tail-bound (cons tail-ty tail-bound) #f)))]))

(define (do-apply-log subst fun arg)
  (match* (fun arg)
          [('star 'list) (printf/log "Polymorphic apply called with uniform rest arg, list argument\n")]
          [('star 'dots) (printf/log "Polymorphic apply called with uniform rest arg, dotted argument\n")]
          [('dots 'dots) (printf/log "Polymorphic apply called with non-uniform rest arg, dotted argument\n")])
  (log-result subst))

(define (tc/apply f args)
  (define f-ty (tc-expr f))
  ;; produces the first n-1 elements of the list, and the last element
  (define (split l)
    (let loop ([l l] [acc '()])
      (if (null? (cdr l))
          (values (reverse acc) (car l))
          (loop (cdr l) (cons (car l) acc)))))
  (define-values (fixed-args tail) (split (syntax->list args)))

  (match f-ty
    [(tc-result: (Function: (list (arr: doms rngs rests drests thn-effs els-effs) ...)))
     (when (null? doms)
       (tc-error/expr #:return (ret (Un))
                      "empty case-lambda given as argument to apply"))
     (let ([arg-tys (map tc-expr/t fixed-args)])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (let-values ([(tail-ty tail-bound)
                              (with-handlers ([exn:fail? (lambda _ (values (tc-expr/t tail) #f))])
                                (tc/dots tail))])
                  (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests arg-tys tail-ty tail-bound))))]
               [(and (car rests*)
                     (let-values ([(tail-ty tail-bound) 
                                   (with-handlers ([exn:fail? (lambda _ (values #f #f))])
                                     (tc/dots tail))])
                       (and tail-ty
                            (subtype (apply -lst* arg-tys #:tail (make-Listof tail-ty))
                                     (apply -lst* (car doms*) #:tail (make-Listof (car rests*)))))))
                (printf/log "Non-poly apply, ... arg\n")
                (ret (car rngs*))]
               [(and (car rests*)
                     (let ([tail-ty (with-handlers ([exn:fail? (lambda _ #f)])
                                      (tc-expr/t tail))])
                       (and tail-ty
                            (subtype (apply -lst* arg-tys #:tail tail-ty)
                                     (apply -lst* (car doms*) #:tail (make-Listof (car rests*)))))))
                
                    (printf/log (if (memq (syntax->datum f) '(+ - * / max min)) 
                                    "Simple arithmetic non-poly apply\n"
                                    "Simple non-poly apply\n"))
                    (ret (car rngs*))]
               [(and (car drests*)
                     (let-values ([(tail-ty tail-bound) 
                                   (with-handlers ([exn:fail? (lambda _ (values #f #f))])
                                     (tc/dots tail))])
                       (and tail-ty
                            (eq? (cdr (car drests*)) tail-bound)
                            (subtypes arg-tys (car doms*))
                            (subtype tail-ty (car (car drests*))))))                     
                (printf/log "Non-poly apply, ... arg\n")
                (ret (car rngs*))]
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests drests thn-effs els-effs) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (with-handlers ([exn:fail:syntax? (lambda _ (values (tc-expr/t tail) #f))])
                                           (tc/dots tail))])
       #;(for-each (lambda (x) (unless (not (Poly? x))                                      
                               (tc-error "Polymorphic argument of type ~a to polymorphic function in apply not allowed" x)))
                 (cons tail-ty arg-tys))
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result: (Poly-names: _ (Function: (list (arr: doms rngs rests drests _ _) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests arg-tys tail-ty tail-bound)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars
                                   (cons tail-ty arg-tys) 
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)
                                   (fv (car rngs*))))
                => (lambda (substitution) (ret (subst-all substitution (car rngs*))))]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound                     
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)
                                   (fv (car rngs*))))
                => (lambda (substitution) (ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg
               [(and (car drests*)
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))
                     (infer vars (cons tail-ty arg-tys) (cons (car (car drests*)) (car doms*)) (car rngs*) (fv (car rngs*))))
                => (lambda (substitution) (ret (subst-all substitution (car rngs*))))]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result: (Poly: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result: (PolyDots: (and vars (list fixed-vars ... dotted-var))
                            (Function: (list (arr: doms rngs rests drests thn-effs els-effs) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (with-handlers ([exn:fail:syntax? (lambda _ (values (tc-expr/t tail) #f))])
                                           (tc/dots tail))])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result: (PolyDots-names: _ (Function: (list (arr: doms rngs rests drests _ _) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests arg-tys tail-ty tail-bound)))])]
               ;; the actual work, when we have a * function and a list final argument
               [(and (car rests*)
                     (not tail-bound)
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars
                                   (cons tail-ty arg-tys) 
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)
                                   (fv (car rngs*))))
                => (lambda (substitution) 
                     (do-apply-log substitution 'star 'list)
                     (ret (subst-all substitution (car rngs*))))]
               ;; actual work, when we have a * function and ... final arg
               [(and (car rests*)
                     tail-bound                     
                     (<= (length (car doms*))
                         (length arg-tys))
                     (infer/vararg vars
                                   (cons (make-Listof tail-ty) arg-tys)
                                   (cons (make-Listof (car rests*))
                                         (car doms*))
                                   (car rests*)
                                   (car rngs*)
                                   (fv (car rngs*))))
                => (lambda (substitution) 
                     (do-apply-log substitution 'star 'dots)
                     (ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg, same bound on ...
               [(and (car drests*)
                     tail-bound
                     (eq? tail-bound (cdr (car drests*)))
                     (= (length (car doms*))
                        (length arg-tys))
                     (infer vars (cons tail-ty arg-tys) (cons (car (car drests*)) (car doms*)) (car rngs*) (fv (car rngs*))))
                => (lambda (substitution)
                     (do-apply-log substitution 'dots 'dots)
                     (ret (subst-all substitution (car rngs*))))]
               ;; ... function, ... arg, different bound on ...
               [(and (car drests*)
                     tail-bound
                     (not (eq? tail-bound (cdr (car drests*))))
                     (= (length (car doms*))
                        (length arg-tys))
                     (parameterize ([current-tvars (extend-env (list tail-bound (cdr (car drests*)))
                                                               (list (make-DottedBoth (make-F tail-bound))
                                                                     (make-DottedBoth (make-F (cdr (car drests*)))))
                                                               (current-tvars))])
                       (infer vars (cons tail-ty arg-tys) (cons (car (car drests*)) (car doms*)) (car rngs*) (fv (car rngs*)))))
                => (lambda (substitution) 
                     (define drest-bound (cdr (car drests*)))
                     (do-apply-log substitution 'dots 'dots)
                     (ret (substitute-dotted (cadr (assq drest-bound substitution))
                                             tail-bound
                                             drest-bound
                                             (subst-all (alist-delete drest-bound substitution eq?)
                                                        (car rngs*)))))]
               ;; if nothing matches, around the loop again
               [else (loop (cdr doms*) (cdr rngs*) (cdr rests*) (cdr drests*))])))]
    [(tc-result: (PolyDots: vars (Function: '())))
     (tc-error/expr #:return (ret (Un))
                    "Function has no cases")]
    [(tc-result: f-ty) (tc-error/expr #:return (ret (Un))
                         "Type of argument to apply is not a function type: ~n~a" f-ty)]))



(define (log-result subst)
  (define (dmap-length d)
    (match d
      [(struct dcon (fixed rest)) (length fixed)]
      [(struct dcon-exact (fixed rest)) (length fixed)]))
  (define (dmap-rest? d)
    (match d
      [(struct dcon (fixed rest)) rest]
      [(struct dcon-exact (fixed rest)) rest]))
  (if (list? subst)
      (for ([s subst])
           (match s
             [(list v (list imgs ...) starred)
              (printf/log "Instantiated ... variable ~a with ~a types\n" v (length imgs))]
             [_ (void)]))
      (for* ([(cmap dmap) (in-pairs (cset-maps subst))]
             [(k v) (dmap-map dmap)])
            (printf/log "Instantiated ... variable ~a with ~a types~a\n" k (dmap-length v)
                        (if (dmap-rest? v)
                            " and a starred type"
                            "")))))

(define-syntax (handle-clauses stx)
  (syntax-case stx ()
    [(_  (lsts ... rngs) pred infer t argtypes expected)
     (with-syntax ([(vars ... rng) (generate-temporaries #'(lsts ... rngs))])
       (syntax/loc stx
         (or (for/or ([vars lsts] ... [rng rngs]
                      #:when (pred vars ... rng))
                     (let ([substitution (infer vars ... rng)])
                       (and substitution
                            (log-result substitution)
                            (or expected
                                (ret (subst-all substitution rng))))))             
             (poly-fail t argtypes))))]))

(define (poly-fail t argtypes)
  (match t
    [(or (Poly-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests _ _) ...)))
         (PolyDots-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests _ _) ...))))
     (if (and (andmap null? msg-doms)
              (null? argtypes))
         (tc-error/expr #:return (ret (-> (Un)))
                        "Could not infer types for applying polymorphic function over ~a~n"
                        (stringify msg-vars))
         (tc-error/expr #:return (ret (->* (list) Univ (Un)))
                        (string-append
                         "Polymorphic function over ~a could not be applied to arguments:~n"
                         (domain-mismatches t msg-doms msg-rests msg-drests argtypes #f #f))
                        (stringify msg-vars)))]))


(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  (match-let* ([(list (tc-result: argtypes arg-thn-effs arg-els-effs) ...) argtys])
    (let outer-loop ([ftype ftype0] 
                     [argtypes argtypes]
                     [arg-thn-effs arg-thn-effs]
                     [arg-els-effs arg-els-effs]
                     [args args-stx])
      (match ftype
        ;; procedural structs
        [(tc-result: (and sty (Struct: _ _ _ (? Type? proc-ty) _ _ _)) thn-eff els-eff)
         (outer-loop (ret proc-ty thn-eff els-eff)
                     (cons (tc-result-t ftype0) argtypes)
                     (cons (list) arg-thn-effs)
                     (cons (list) arg-els-effs)
                     #`(#,(syntax/loc f-stx dummy) #,@args))]
        ;; mu types, etc
        [(tc-result: (? needs-resolving? t) thn-eff els-eff)
         (outer-loop (ret (resolve-once t) thn-eff els-eff) argtypes arg-thn-effs arg-els-effs args)]
        ;; parameters
        [(tc-result: (Param: in out))
         (match argtypes
           [(list) (ret out)]
           [(list t)
            (if (subtype t in) 
                (ret -Void)
                (tc-error/expr #:return (ret (Un))
                               "Wrong argument to parameter - expected ~a and got ~a" in t))]
           [_ (tc-error/expr #:return (ret (Un))
                             "Wrong number of arguments to parameter - expected 0 or 1, got ~a"
                             (length argtypes))])]
        ;; single clause functions
        [(tc-result: (and t (Function: (list (arr: dom rng rest #f latent-thn-effs latent-els-effs))))
                     thn-eff els-eff)
         (let-values ([(thn-eff els-eff)
                       (tc-args argtypes arg-thn-effs arg-els-effs dom rest 
                                latent-thn-effs latent-els-effs
                                (syntax->list args))])
           (ret rng thn-eff els-eff))]
        ;; non-polymorphic case-lambda functions
        [(tc-result: (and t (Function: (list (arr: doms rngs rests (and drests #f) latent-thn-effs latent-els-effs) ..1)))
                     thn-eff els-eff)
         (let loop ([doms* doms] [rngs rngs] [rests* rests])
           (cond [(null? doms*) 
                  (tc-error/expr 
                   #:return (ret (Un))
                   (string-append "No function domains matched in function application:"
                                  (domain-mismatches t doms rests drests argtypes #f #f)))]
                 [(subtypes/varargs argtypes (car doms*) (car rests*)) 
                  (when (car rests*)
                    (printf/log "Simple varargs function application (~a)\n" (syntax->datum f-stx)))
                  (ret (car rngs))]
                 [else (loop (cdr doms*) (cdr rngs) (cdr rests*))]))]
        ;; simple polymorphic functions, no rest arguments
        [(tc-result: (and t
                          (or (Poly: vars 
                                     (Function: (list (arr: doms rngs (and rests #f) (and drests #f) thn-effs els-effs) ...)))
                              (PolyDots: (list vars ... _)
                                         (Function: (list (arr: doms rngs (and rests #f) (and drests #f) thn-effs els-effs) ...))))))
         (handle-clauses (doms rngs) 
                         (lambda (dom _) (= (length dom) (length argtypes)))
                         (lambda (dom rng) (infer (fv/list (cons rng dom)) argtypes dom rng (fv rng) expected))
                         t argtypes expected)]
        ;; polymorphic varargs
        [(tc-result: (and t
                          (or (Poly: vars (Function: (list (arr: doms rngs rests (and drests #f) thn-effs els-effs) ...)))
                              ;; we want to infer the dotted-var here as well, and we don't use these separately
                              ;; so we can just use "vars" instead of (list fixed-vars ... dotted-var)
                              (PolyDots: vars (Function: (list (arr: doms rngs rests (and drests #f) thn-effs els-effs) ...))))))
         (printf/log "Polymorphic varargs function application (~a)\n" (syntax->datum f-stx))
         (handle-clauses (doms rests rngs)
                         (lambda (dom rest rng) (<= (length dom) (length argtypes)))
                         (lambda (dom rest rng) (infer/vararg vars argtypes dom rest rng (fv rng) expected))
                         t argtypes expected)]
        ;; polymorphic ... type
        [(tc-result: (and t (PolyDots: 
                             (and vars (list fixed-vars ... dotted-var))
                             (Function: (list (arr: doms rngs (and #f rests) (cons dtys dbounds) thn-effs els-effs) ...)))))
         (printf/log "Polymorphic ... function application (~a)\n" (syntax->datum f-stx))
         (handle-clauses (doms dtys dbounds rngs)
                         (lambda (dom dty dbound rng) (and (<= (length dom) (length argtypes))
                                                           (eq? dotted-var dbound)))
                         (lambda (dom dty dbound rng) (infer/dots fixed-vars dotted-var argtypes dom dty rng (fv rng) expected))
                         t argtypes expected)]
        ;; Union of function types works if we can apply all of them
        [(tc-result: (Union: (list (and fs (Function: _)) ...)) e1 e2)
         (match-let ([(list (tc-result: ts) ...) (map (lambda (f) (outer-loop 
                                                                   (ret f e1 e2) argtypes arg-thn-effs arg-els-effs args)) fs)])
           (ret (apply Un ts)))]
        [(tc-result: f-ty _ _) (tc-error/expr #:return (ret (Un))
                                              "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))))

;(trace tc/funapp)

(define (tc/app form) (tc/app/internal form #f))

(define (tc/app/check form expected)
  (define t (tc/app/internal form expected))
  (check-below t expected)
  (ret expected))

;; expr id -> type or #f
;; if there is a binding in stx of the form:
;; (let ([x (reverse name)]) e)
;; where x has a type annotation, return that annotation, otherwise #f
(define (find-annotation stx name)
  (define (find s) (find-annotation s name))
  (define (match? b)
    (kernel-syntax-case* b #f (reverse)
      [[(v) (#%plain-app reverse n)]
       (free-identifier=? name #'n)
       (begin ;(printf "found annotation: ~a ~a~n~a~n" (syntax-e name) (syntax-e #'v) (type-annotation #'v))
              (type-annotation #'v))]
      [_ #f]))
  (kernel-syntax-case* 
      stx #f (reverse letrec-syntaxes+values)
    [(let-values (binding ...) body)
     (cond [(ormap match? (syntax->list #'(binding ...)))]
           [else (find #'body)])]
    [(#%plain-app e ...) (ormap find (syntax->list #'(e ...)))]
    [(if e1 e2 e3) (ormap find (syntax->list #'(e1 e2 e3)))]
    [(letrec-values ([(v ...) e] ...) b)
     (ormap find (syntax->list #'(e ... b)))]
    [(letrec-syntaxes+values _ ([(v ...) e] ...) b)
     (ormap find (syntax->list #'(e ... b)))]
    [(begin . es)
     (ormap find (syntax->list #'es))]
    [(#%plain-lambda (v ...) e)
     (find #'e)]
    [_ #f]))


(define (check-do-make-object cl pos-args names named-args)
  (let* ([names (map syntax-e (syntax->list names))]
         [name-assoc (map list names (syntax->list named-args))])
    (let loop ([t (tc-expr cl)])
      (match t
        [(tc-result: (? Mu? t)) (loop (ret (unfold t)))]
        [(tc-result: (and c (Class: pos-tys (list (and tnflds (list tnames _ _)) ...) _))) 
         (unless (= (length pos-tys)
                    (length (syntax->list pos-args)))
           (tc-error/delayed "expected ~a positional arguments, but got ~a"
                             (length pos-tys) (length (syntax->list pos-args))))
         ;; use for, since they might be different lengths in error case
         (for ([pa (in-syntax pos-args)]
               [pt (in-list pos-tys)])
           (tc-expr/check pa pt))
         (for ([n names]
               #:when (not (memq n tnames)))
           (tc-error/delayed 
            "unknown named argument ~a for class~nlegal named arguments are ~a"
            n (stringify tnames)))
         (for-each (match-lambda
                     [(list tname tfty opt?)
                      (let ([s (cond [(assq tname name-assoc) => cadr]
                                     [(not opt?)
                                      (tc-error/delayed "value not provided for named init arg ~a" tname)
                                      #f]
                                     [else #f])])
                        (if s
                            ;; this argument was present
                            (tc-expr/check s tfty)
                            ;; this argument wasn't provided, and was optional
                            #f))])
                   tnflds)
         (ret (make-Instance c))]
        [(tc-result: t)
         (tc-error/expr #:return (ret (Un)) "expected a class value for object creation, got: ~a" t)]))))

(define (tc/app/internal form expected)
  (kernel-syntax-case* form #f 
    (values apply not list list* call-with-values do-make-object make-object cons
            andmap ormap) ;; the special-cased functions   
    ;; special cases for classes
    [(#%plain-app make-object cl . args)     
     (check-do-make-object #'cl #'args #'() #'())]
    [(#%plain-app do-make-object cl (#%plain-app list . pos-args) (#%plain-app list (#%plain-app cons 'names named-args) ...))
     (check-do-make-object #'cl #'pos-args #'(names ...) #'(named-args ...))]
    [(#%plain-app do-make-object . args)
     (int-err "bad do-make-object : ~a" (syntax->datum #'args))]
    ;; call-with-values
    [(#%plain-app call-with-values prod con)
     (match-let* ([(tc-result: prod-t) (tc-expr #'prod)])
       (define (values-ty->list t)
         (match t
           [(Values: ts) ts]
           [_ (list t)]))
       (match prod-t
         [(Function: (list (arr: (list) vals _ #f _ _)))
          (tc/funapp #'con #'prod (tc-expr #'con) (map ret (values-ty->list vals)) expected)]
         [_ (tc-error/expr #:return (ret (Un)) 
                           "First argument to call with values must be a function that can accept no arguments, got: ~a"
                           prod-t)]))]
    ;; special cases for `values'    
    ;; special case the single-argument version to preserve the effects
    [(#%plain-app values arg) (tc-expr #'arg)]
    [(#%plain-app values . args)
     (let ([tys (map tc-expr/t (syntax->list #'args))])
       (ret (-values tys)))]
    ;; special case for `list'
    [(#%plain-app list . args)
     (let ([tys (map tc-expr/t (syntax->list #'args))])
       (ret (apply -lst* tys)))]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                  [tys (reverse tys-r)])
       (ret (foldr make-Pair last tys)))]
    ;; in eq? cases, call tc/eq
    [(#%plain-app eq? v1 v2) 
     (and (identifier? #'eq?) (comparator? #'eq?))
     (begin
       ;; make sure the whole expression is type correct
       (tc/funapp #'eq? #'(v1 v2) (tc-expr #'eq?) (map tc-expr (syntax->list #'(v1 v2))) expected)
       ;; check thn and els with the eq? info
       (let-values ([(thn-eff els-eff) (tc/eq #'eq? #'v1 #'v2)])
         (ret B thn-eff els-eff)))]
    ;; special case for `not'
    [(#%plain-app not arg)
     (match (tc-expr #'arg)
       ;; if arg was a predicate application, we swap the effects
       [(tc-result: t thn-eff els-eff)
        (ret B (map var->type-eff els-eff) (map var->type-eff thn-eff))])]
    ;; special case for `apply'
    [(#%plain-app apply f . args) (tc/apply #'f #'args)]    
    ;; even more special case for match
    [(#%plain-app (letrec-values ([(lp) (#%plain-lambda args . body)]) lp*) . actuals)
     (and expected (not (andmap type-annotation (syntax->list #'args))) (free-identifier=? #'lp #'lp*))
     (let-loop-check #'form #'lp #'actuals #'args #'body expected)]
    ;; or/andmap of ... argument
    [(#%plain-app or/andmap f arg)
     (and 
      (identifier? #'or/andmap)
      (or (free-identifier=? #'or/andmap #'ormap)
          (free-identifier=? #'or/andmap #'andmap))
      (with-handlers ([exn:fail? (lambda _ #f)])
        (tc/dots #'arg)
        #t))
     (let-values ([(ty bound) (tc/dots #'arg)])
       (parameterize ([current-tvars (extend-env (list bound)
                                                 (list (make-DottedBoth (make-F bound)))
                                                 (current-tvars))])
         (match-let* ([ft (tc-expr #'f)]
                      [(tc-result: t) (tc/funapp #'f #'(arg) ft (list (ret ty)) #f)])
           (ret (Un (-val #f) t)))))]
    ;; default case
    [(#%plain-app f args ...) 
     (tc/funapp #'f #'(args ...) (tc-expr #'f) (map tc-expr (syntax->list #'(args ...))) expected)]))

(define (let-loop-check form lp actuals args body expected)
  (kernel-syntax-case* #`(#,args #,body #,actuals) #f (null?)
    [((val acc ...)
      ((if (#%plain-app null? val*) thn els))
      (actual actuals ...))
     (and (free-identifier=? #'val #'val*)
          (ormap (lambda (a) (find-annotation #'(if (#%plain-app null? val*) thn els) a))
                 (syntax->list #'(acc ...))))
     (let* ([ts1 (generalize (tc-expr/t #'actual))]
            [ann-ts (for/list ([a (in-syntax #'(acc ...))]
                               [ac (in-syntax #'(actuals ...))])
                      (or (find-annotation #'(if (#%plain-app null? val*) thn els) a)
                          (generalize (tc-expr/t ac))))]
            [ts (cons ts1 ann-ts)])
       ;; check that the actual arguments are ok here
       (map tc-expr/check (syntax->list #'(actuals ...)) ann-ts)
       ;; then check that the function typechecks with the inferred types
       (tc/rec-lambda/check form args body lp ts expected)
       (ret expected))]
    ;; special case when argument needs inference
    [_
     (let ([ts (map (compose generalize tc-expr/t) (syntax->list actuals))])
       (tc/rec-lambda/check form args body lp ts expected)
       (ret expected))]))

(define (matches? stx)
  (let loop ([stx stx] [ress null] [acc*s null])
    (syntax-case stx (#%plain-app reverse)
      [([(res) (#%plain-app reverse acc*)] . more)
       (loop #'more (cons #'res ress) (cons #'acc* acc*s))]
      [rest
       (syntax->list #'rest)
       (begin
         ;(printf "ress: ~a~n" (map syntax-e ress))
         (list (reverse ress) (reverse acc*s) #'rest))]
      [_ #f])))

;(trace tc/app/internal)
