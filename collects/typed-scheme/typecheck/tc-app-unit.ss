#lang scheme/unit

(require (only-in "../utils/utils.ss" debug in-syntax printf/log in-pairs rep utils private env [infer r:infer]))
(require "signatures.ss"
         stxclass
         (for-syntax stxclass)
         (rep type-rep effect-rep)
	 (utils tc-utils)
	 (private subtype type-utils union type-effect-convenience type-effect-printer resolve-type
		  type-annotation)
         (r:infer infer)
	 (env type-environments)
         (only-in srfi/1 alist-delete)
         (only-in scheme/private/class-internal make-object do-make-object)
         mzlib/trace mzlib/pretty syntax/kerncase scheme/match
         (prefix-in c: scheme/contract)
         (for-syntax scheme/base)
         (for-template 
          (only-in '#%kernel [apply k:apply])
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))
(require (r:infer constraint-structs))

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

(define (stringify-domain dom rst drst [rng #f])
  (let ([doms-string (if (null? dom) "" (string-append (stringify dom) " "))]
        [rng-string (if rng (format " -> ~a" rng) "")])
    (cond [drst
           (format "~a~a ... ~a~a" doms-string (car drst) (cdr drst) rng-string)]
          [rst
           (format "~a~a *~a" doms-string rst rng-string)]
          [else (string-append (stringify dom) rng-string)])))

(define (domain-mismatches ty doms rests drests rngs arg-tys tail-ty tail-bound #:expected [expected #f])
  (define arguments-str
    (stringify-domain arg-tys (if (not tail-bound) tail-ty #f) (if tail-bound (cons tail-ty tail-bound) #f)))    
  (cond
    [(null? doms)
     (int-err "How could doms be null: ~a ~a" ty)]
    [(= 1 (length doms))
     (format "Domain: ~a~nArguments: ~a~n~a"
             (stringify-domain (car doms) (car rests) (car drests))
             arguments-str
             (if expected
                 (format "Result type: ~a~nExpected result: ~a~n"
                         (car rngs) expected)
                 ""))]
    [else
     (format "~a: ~a~nArguments: ~a~n~a"
             (if expected "Types" "Domains")
             (stringify (if expected 
                            (map stringify-domain doms rests drests rngs)
                            (map stringify-domain doms rests drests))
                        "~n\t")
             arguments-str
             (if expected
                 (format "Expected result: ~a~n" expected)
                 ""))]))

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
    [(tc-result: (Function: (list (arr: doms rngs rests drests '() thn-effs els-effs) ...)))
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
                                  (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty tail-bound))))]
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
    [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests drests '() thn-effs els-effs) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (with-handlers ([exn:fail:syntax? (lambda _ (values (tc-expr/t tail) #f))])
                                           (tc/dots tail))])
       #;(for-each (lambda (x) (unless (not (Poly? x))                                      
                               (tc-error "Polymorphic argument of type ~a to polymorphic function in apply not allowed" x)))
                 (cons tail-ty arg-tys))
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result: (Poly-names: _ (Function: (list (arr: doms rngs rests drests '() _ _) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty tail-bound)))])]
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
                            (Function: (list (arr: doms rngs rests drests '() thn-effs els-effs) ..1))))
     (let*-values ([(arg-tys) (map tc-expr/t fixed-args)]
                   [(tail-ty tail-bound) (with-handlers ([exn:fail:syntax? (lambda _ (values (tc-expr/t tail) #f))])
                                           (tc/dots tail))])
       (let loop ([doms* doms] [rngs* rngs] [rests* rests] [drests* drests])
         (cond [(null? doms*)
                (match f-ty 
                  [(tc-result: (PolyDots-names: _ (Function: (list (arr: doms rngs rests drests '() _ _) ..1))))
                   (tc-error/expr #:return (ret (Un))
                                 (string-append 
                                  "Bad arguments to polymorphic function in apply:~n"
                                  (domain-mismatches f-ty doms rests drests rngs arg-tys tail-ty tail-bound)))])]
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
               ;; ... function, (List A B C etc) arg
               [(and (car drests*)
                     (not tail-bound)
                     (eq? (cdr (car drests*)) dotted-var)
                     (= (length (car doms*))
                        (length arg-tys))
                     (untuple tail-ty)
                     (infer/dots fixed-vars dotted-var (append arg-tys (untuple tail-ty)) (car doms*)
                                 (car (car drests*)) (car rngs*) (fv (car rngs*))))
                => (lambda (substitution) 
                     (define drest-bound (cdr (car drests*)))
                     (do-apply-log substitution 'dots 'dots)
                     (ret (subst-all substitution (car rngs*))))]
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
  (syntax-parse stx
    [(_  (lsts ... rngs) f-stx pred infer t argtypes expected)
     (with-syntax ([(vars ... rng) (generate-temporaries #'(lsts ... rngs))])
       (syntax/loc stx
         (or (for/or ([vars lsts] ... [rng rngs]
                      #:when (pred vars ... rng))
                     (let ([substitution (infer vars ... rng)])
                       (and substitution
                            (log-result substitution)
                            (ret (or expected
                                     (subst-all substitution rng))))))
             (poly-fail t argtypes #:name (and (identifier? f-stx) f-stx) #:expected expected))))]))

(define (poly-fail t argtypes #:name [name #f] #:expected [expected #f])
  (match t
    [(or (Poly-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests '() _ _) ...)))
         (PolyDots-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests '() _ _) ...))))
     (let ([fcn-string (if name
                           (format "function ~a" (syntax->datum name))
                           "function")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr #:return (ret (Un))
                          (string-append 
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (tc-error/expr #:return (ret (Un))
                          (string-append
                           "Polymorphic " fcn-string " could not be applied to arguments:~n"
                           (domain-mismatches t msg-doms msg-rests msg-drests msg-rngs argtypes #f #f #:expected expected)
                           (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                               (string-append "Type Variables: " (stringify msg-vars) "\n")
                               "")))))]))


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
        ;; FIXME - error on non-optional keywords
        [(tc-result: (and t (Function: (list (arr: dom rng rest #f _ latent-thn-effs latent-els-effs))))
                     thn-eff els-eff)
         (let-values ([(thn-eff els-eff)
                       (tc-args argtypes arg-thn-effs arg-els-effs dom rest 
                                latent-thn-effs latent-els-effs
                                (syntax->list args))])
           (ret rng thn-eff els-eff))]
        ;; non-polymorphic case-lambda functions
        [(tc-result: (and t (Function: (list (arr: doms rngs rests (and drests #f) '() latent-thn-effs latent-els-effs) ..1)))
                     thn-eff els-eff)
         (let loop ([doms* doms] [rngs rngs] [rests* rests])
           (cond [(null? doms*) 
                  (tc-error/expr 
                   #:return (ret (Un))
                   (string-append "No function domains matched in function application:\n"
                                  (domain-mismatches t doms rests drests rngs argtypes #f #f)))]
                 [(subtypes/varargs argtypes (car doms*) (car rests*)) 
                  (when (car rests*)
                    (printf/log "Simple varargs function application (~a)\n" (syntax->datum f-stx)))
                  (ret (car rngs))]
                 [else (loop (cdr doms*) (cdr rngs) (cdr rests*))]))]
        ;; simple polymorphic functions, no rest arguments
        [(tc-result: (and t
                          (or (Poly: vars 
                                     (Function: (list (arr: doms rngs (and rests #f) (and drests #f) '() thn-effs els-effs) ...)))
                              (PolyDots: (list vars ...)
                                         (Function: (list (arr: doms rngs (and rests #f) (and drests #f) '() thn-effs els-effs) ...))))))
         (handle-clauses (doms rngs) f-stx
                         (lambda (dom _) (= (length dom) (length argtypes)))
                         (lambda (dom rng) (infer vars argtypes dom rng (fv rng) expected))
                         t argtypes expected)]
        ;; polymorphic varargs
        [(tc-result: (and t
                          (or (Poly: vars (Function: (list (arr: doms rngs rests (and drests #f) '() thn-effs els-effs) ...)))
                              ;; we want to infer the dotted-var here as well, and we don't use these separately
                              ;; so we can just use "vars" instead of (list fixed-vars ... dotted-var)
                              (PolyDots: vars (Function: (list (arr: doms rngs rests (and drests #f) '() thn-effs els-effs) ...))))))
         (printf/log "Polymorphic varargs function application (~a)\n" (syntax->datum f-stx))
         (handle-clauses (doms rests rngs) f-stx
                         (lambda (dom rest rng) (<= (length dom) (length argtypes)))
                         (lambda (dom rest rng) (infer/vararg vars argtypes dom rest rng (fv rng) expected))
                         t argtypes expected)]
        ;; polymorphic ... type
        [(tc-result: (and t (PolyDots: 
                             (and vars (list fixed-vars ... dotted-var))
                             (Function: (list (arr: doms rngs (and #f rests) (cons dtys dbounds) '() thn-effs els-effs) ...)))))
         (printf/log "Polymorphic ... function application (~a)\n" (syntax->datum f-stx))
         (handle-clauses (doms dtys dbounds rngs) f-stx
                         (lambda (dom dty dbound rng) (and (<= (length dom) (length argtypes))
                                                           (eq? dotted-var dbound)))
                         (lambda (dom dty dbound rng) 
                           (infer/dots fixed-vars dotted-var argtypes dom dty rng (fv rng) #:expected expected))
                         t argtypes expected)]
        ;; Union of function types works if we can apply all of them
        [(tc-result: (Union: (list (and fs (Function: _)) ...)) e1 e2)
         (match-let ([(list (tc-result: ts) ...) (map (lambda (f) (outer-loop 
                                                                   (ret f e1 e2) argtypes arg-thn-effs arg-els-effs args)) fs)])
           (ret (apply Un ts)))]
        ;; error type is a perfectly good fcn type
        [(tc-result: (Error:)) (ret (make-Error))]
        [(tc-result: f-ty _ _) (tc-error/expr #:return (ret (Un))
                                              "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))))

;(trace tc/funapp)



(define (tc/app form) (tc/app/internal form #f))  
  
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected)
    (ret expected))

(define-syntax-class lv-clause
  #:transparent
  (pattern [(v:id ...) e:expr]))

(define-syntax-class lv-clauses
  #:transparent
  (pattern (cl:lv-clause ...)
           #:with (e ...) #'(cl.e ...)
           #:with (vs ...) #'((cl.v ...) ...)))

(define-syntax-class core-expr
  #:literals (reverse letrec-syntaxes+values let-values #%plain-app
                      if letrec-values begin #%plain-lambda set! case-lambda
                      begin0 with-continuation-mark)
  #:transparent
  (pattern (let-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-syntaxes+values _ cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (#%plain-app expr ...))
  (pattern (if expr ...))
  (pattern (with-continuation-mark expr ...))
  (pattern (begin expr ...))
  (pattern (begin0 expr ...))
  (pattern (#%plain-lambda _ e)
           #:with (expr ...) #'(e))
  (pattern (case-lambda [_ expr] ...))
  (pattern (set! _ e)
           #:with (expr ...) #'(e))
  (pattern _ 
           #:with (expr ...) #'()))

;; expr id -> type or #f
;; if there is a binding in stx of the form:
;; (let ([x (reverse name)]) e)
;; where x has a type annotation, return that annotation, otherwise #f
(define (find-annotation stx name)
  (define (find s) (find-annotation s name))
  (define (match? b)
    (syntax-parse b
      #:literals (#%plain-app reverse)
      [c:lv-clause
       #:with (#%plain-app reverse n:id) #'c.e
       #:with (v) #'(c.v ...) 
       #:when (free-identifier=? name #'v)
       (type-annotation #'v)]
      [_ #f]))
  (syntax-parse stx
    #:literals (let-values)
    [(let-values cls:lv-clauses body)
     (or (ormap match? (syntax->list #'cls))
	 (find #'body))]
    [e:core-expr
     (ormap find (syntax->list #'(e.expr ...)))]))


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

(define (tc-keywords form arities kws kw-args pos-args expected)
  (match arities
    [(list (arr: dom rng rest #f ktys _ _))
     ;; assumes that everything is in sorted order
     (let loop ([actual-kws kws]
                [actuals (map tc-expr/t (syntax->list kw-args))]
                [formals ktys])
       (match* (actual-kws formals)
         [('() '())
          (void)]
         [(_ '())
          (tc-error/expr #:return (ret (Un))
                         "Unexpected keyword argument ~a" (car actual-kws))]
         [('() (cons fst rst))
          (match fst
            [(Keyword: k _ #t)
             (tc-error/expr #:return (ret (Un))
                            "Missing keyword argument ~a" k)]
            [_ (loop actual-kws actuals rst)])]
         [((cons k kws-rest) (cons (Keyword: k* t req?) form-rest))
          (cond [(eq? k k*) ;; we have a match
                 (unless (subtype (car actuals) t)
                   (tc-error/delayed
                    "Wrong function argument type, expected ~a, got ~a for keyword argument ~a"
                    t (car actuals) k))
                 (loop kws-rest (cdr actuals) form-rest)]
                [req? ;; this keyword argument was required
                 (tc-error/delayed "Missing keyword argument ~a" k*)
                 (loop kws-rest (cdr actuals) form-rest)]
                [else ;; otherwise, ignore this formal param, and continue
                 (loop actual-kws actuals form-rest)])]))
     (tc/funapp (car (syntax-e form)) kw-args (ret (make-Function arities)) (map tc-expr (syntax->list pos-args)) expected)]
    [_ (int-err "case-lambda w/ keywords not supported")]))


(define (type->list t)
  (match t
    [(Pair: (Value: (? keyword? k)) b) (cons k (type->list b))]
    [(Value: '()) null]
    [_ (int-err "bad value in type->list: ~a" t)]))

;; id: identifier
;; sym: a symbol
;; mod: a quoted require spec like 'scheme/base
;; is id the name sym defined in mod?
(define (id-from? id sym mod)
  (and (eq? (syntax-e id) sym)
       (eq? (module-path-index-resolve (syntax-source-module id))
            ((current-module-name-resolver) mod #f #f #f))))

(define (tc/app/internal form expected)
  (kernel-syntax-case* form #f 
    (values apply k:apply not list list* call-with-values do-make-object make-object cons
            andmap ormap) ;; the special-cased functions
    ;; special case for delay
    [(#%plain-app 
      mp1 
      (#%plain-lambda () 
        (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
     (and (id-from? #'mp1 'make-promise 'scheme/promise)
          (id-from? #'mp2 'make-promise 'scheme/promise))
     (ret (-Promise (tc-expr/t #'e)))]
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
         [(Function: (list (arr: (list) vals _ #f '()  _ _)))
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
    [(#%plain-app k:apply . args)
     (tc/app/internal #'(#%plain-app apply . args) expected)]
    ;; special-er case for (apply values (list x y z))
    [(#%plain-app apply values e)
     (cond [(with-handlers ([exn:fail? (lambda _ #f)])
               (untuple (tc-expr/t #'e)))
            => (lambda (t) (ret (-values t)))]
           [else (tc/apply #'values #'(e))])]
    ;; special case for `apply'
    [(#%plain-app apply f . args) (tc/apply #'f #'args)]
    ;; special case for keywords
    [(#%plain-app
      (#%plain-app kpe kws num fn)
      kw-list
      (#%plain-app list . kw-arg-list)
      . pos-args)
     (eq? (syntax-e #'kpe) 'keyword-procedure-extract)
     (match (tc-expr #'fn)
       [(tc-result: (Function: arities)) 
        (tc-keywords form arities (type->list (tc-expr/t #'kws)) #'kw-arg-list #'pos-args expected)]
       [t (tc-error/expr #:return (ret (Un))
                         "Cannot apply expression of type ~a, since it is not a function type" t)])]
    ;; even more special case for match
    [(#%plain-app (letrec-values ([(lp) (#%plain-lambda args . body)]) lp*) . actuals)
     (and expected (not (andmap type-annotation (syntax->list #'args))) (free-identifier=? #'lp #'lp*))
     (let-loop-check form #'lp #'actuals #'args #'body expected)]
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
