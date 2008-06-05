#lang scheme/unit

(require "signatures.ss"
         "type-rep.ss"
         "effect-rep.ss"
         "tc-utils.ss"
         "subtype.ss"
         "unify.ss"
         "infer-ops.ss"
         "union.ss"
         "type-utils.ss"
         "type-effect-convenience.ss"
         "type-effect-printer.ss"
         "type-annotation.ss"
         "resolve-type.ss"
         (only-in scheme/private/class-internal make-object do-make-object)
         mzlib/trace mzlib/pretty syntax/kerncase scheme/match
         (for-template 
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^)
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
;; [Type] [Type] Maybe[Type] [Syntax] -> Effect  
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
           #;(printf "got to mi= ~a ~a ~n~a ~a~n" 
                     (var-true-effect-v (caar arg-thn-effs)) (var-true-effect-v (caar arg-els-effs))
                     (syntax-e (var-true-effect-v (caar arg-thn-effs))) (syntax-e (var-false-effect-v (caar arg-els-effs))))
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
           (values null null)]))))


;(trace tc-args)

(define (tc/apply f args)
  (let* ([f-ty (tc-expr f)]
         [arg-tys0 (map tc-expr/t (syntax->list args))])
    ;; produces the first n-1 elements of the list, and the last element
    (define (split l)
      (let loop ([l l] [acc '()])
        (if (null? (cdr l))
            (values (reverse acc) (car l))
            (loop (cdr l) (cons (car l) acc)))))
    (let-values ([(arg-tys tail-ty) (split arg-tys0)])
      (define (printable dom rst)
        (list dom rst '..))
      (match f-ty
        [(tc-result: (Function: (list (arr: doms rngs rests thn-effs els-effs) ..1)))
         (let loop ([doms* doms] [rngs* rngs] [rests* rests])
           (cond [(null? doms*) 
                  (if (and (not (null? doms)) (null? (cdr doms)))
                      (tc-error/expr
                       #:return (ret (Un))
                       "bad arguments to apply - function expected ~a fixed arguments and (Listof ~a) rest argument, given ~a"
                       (car doms) (car rests) arg-tys0)
                      (tc-error/expr
                       #:return (ret (Un))
                       "no function domain matched - domains were: ~a arguments were ~a" 
                       (map printable doms rests)
                       arg-tys0))]
                 [(and (subtypes arg-tys (car doms*)) (car rests*) (subtype tail-ty (make-Listof (car rests*))))
                  (ret (car rngs*))]
                 [else (loop (cdr doms*) (cdr rngs*) (cdr rests*))]))]                      
        [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests thn-effs els-effs) ..1))))
         (for-each (lambda (x) (unless (not (Poly? x))                                      
                                 (tc-error "Polymorphic argument ~a to polymorphic function in apply not allowed" x)))
                   arg-tys0)
         (let loop ([doms* doms] [rngs* rngs] [rests* rests])
           (cond [(null? doms*)
                  (match f-ty 
                    [(tc-result: (Poly-names: vars (Function: (list (arr: doms rngs rests thn-effs els-effs) ..1))))
                     (cond 
                       [(null? doms) (int-err "how could doms be null: ~a ~a" doms f-ty)]
                       [(= 1 (length doms))
                        (if (car rests)
                            (tc-error/expr
                             #:return (ret (Un))
                             "polymorphic function domain did not match -~ndomain was: ~a~nrest argument was: ~a~narguments were ~a~n" 
                             (car doms) (car rests) (stringify arg-tys0))
                            (tc-error/expr
                             #:return (ret (Un))
                             "polymorphic function domain did not match -~ndomain was: ~a~narguments were ~a~n" 
                             (car doms) (stringify arg-tys0)))]
                       [else 
                        (tc-error/expr
                         #:return (ret (Un))
                         "no polymorphic function domain matched - ~ndomains were: ~a~narguments were ~a~n"
                         (stringify 
                          (for/list ([dom doms] [rest rests])
                            (if rest
                                (format "~a rest argument: " (stringify dom) rest)
                                (stringify dom)))
                          "\n")
                         (stringify arg-tys0))])])]
                 [(and (= (length (car doms*))
                          (length arg-tys))
                       (infer vars arg-tys0 (append (car doms*) (list (make-Listof (car rests*)))) (car rngs*)))
                  => (lambda (substitution) 
                       (let* ([s (lambda (t) (subst-all substitution t))]
                              [new-doms* (append (map s (car doms*)) (list (make-Listof (s (car rests*)))))])
                         (unless (andmap subtype arg-tys0 new-doms*)
                           (int-err "Inconsistent substitution - arguments not subtypes: ~n~a~n~a~n" arg-tys0 new-doms*)))
                       (ret (subst-all substitution (car rngs*))))]
                 [else (loop (cdr doms*) (cdr rngs*) (cdr rests*))]))]
        [(tc-result: (Poly: vars (Function: '())))
         (tc-error/expr #:return (ret (Un))
                        "Function has no cases")]
        [f-ty (tc-error/expr #:return (ret (Un))
                             "Type of argument to apply is not a function type: ~n~a" f-ty)]))))


(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  (match-let* ([(list (tc-result: argtypes arg-thn-effs arg-els-effs) ...) argtys])
    (let outer-loop ([ftype ftype0] 
                     [argtypes argtypes]
                     [arg-thn-effs arg-thn-effs]
                     [arg-els-effs arg-els-effs]
                     [args args-stx])
      (match ftype
        [(tc-result: (and sty (Struct: _ _ _ (? Type? proc-ty) _ _ _)) thn-eff els-eff)
         (outer-loop (ret proc-ty thn-eff els-eff)
                     (cons (tc-result-t ftype0) argtypes)
                     (cons (list) arg-thn-effs)
                     (cons (list) arg-els-effs)
                     #`(#,(syntax/loc f-stx dummy) #,@args))]
        [(tc-result: (? needs-resolving? t) thn-eff els-eff)
         (outer-loop (ret (resolve-once t) thn-eff els-eff) argtypes arg-thn-effs arg-els-effs args)]
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
        [(tc-result: (Function: (list (arr: doms rngs rests latent-thn-effs latent-els-effs) ..1)) thn-eff els-eff)
         (if (= 1 (length doms))
             (let-values ([(thn-eff els-eff)
                           (tc-args argtypes arg-thn-effs arg-els-effs (car doms) (car rests) 
                                    (car latent-thn-effs) (car latent-els-effs)
                                    (syntax->list args))])
               (ret (car rngs) thn-eff els-eff)
               #;(if (false-effect? eff)
                     (ret (-val #f) eff)
                     (ret (car rngs) eff)))
             (let loop ([doms* doms] [rngs rngs] [rests* rests])
               (cond [(null? doms*) 
                      (tc-error/expr 
                       #:return (ret (Un))
                       "no function domain matched - domains were:~n~a~narguments were ~a" 
                       (stringify
                        (for/list ([d doms] [r rests])
                          (format "~a ~a*"(stringify d) r))
                        "\n")
                       argtypes)]
                     [(subtypes/varargs argtypes (car doms*) (car rests*)) (ret (car rngs))]
                     [else (loop (cdr doms*) (cdr rngs) (cdr rests*))])))]
        [(and rft (tc-result: (Poly: vars (Function: (list (arr: doms rngs #f thn-effs els-effs) ...)))))
         ;(printf "Typechecking poly app~nftype:          ~a~n" ftype)
         ;(printf "ftype again:    ~a~n" ftype)
         ;(printf "resolved ftype: ~a : ~a~n" (equal? rft ftype) rft)
         ;(printf "reresolving: ~a~n" (resolve-tc-result ftype))
         ;(printf "argtypes: ~a~ndoms: ~a~n" argtypes doms)
         (for-each (lambda (x) (unless (not (Poly? x))
                                 (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                   argtypes)
         (let loop ([doms* doms] [rngs* rngs])
           (cond [(null? doms*)
                  (match-let ([(tc-result: (Poly-names: msg-vars (Function: (list (arr: msg-doms msg-rngs #f _ _) ...)))) ftype])
                    (if (= 1 (length doms))
                        (tc-error/expr #:return (ret (Un)) 
                                       "Polymorphic function could not be applied to arguments:~nExpected: ~a ~nActual: ~a" 
                                       (stringify (car msg-doms)) (stringify argtypes))
                        (tc-error/expr #:return (ret (Un))
                                       "no polymorphic function domain matched - possible domains were: ~n~a~narguments were: ~n~a"
                                       (stringify (map stringify msg-doms) "\n") (stringify argtypes))))]
                 [(and (= (length (car doms*))
                          (length argtypes))
                       (infer (fv/list (cons (car rngs*) (car doms*))) argtypes (car doms*) (car rngs*) expected))
                  => (lambda (substitution)
                       (or expected
                           (let* ([s (lambda (t) (subst-all substitution t))]
                                  [new-doms* (map s (car doms*))])
                             (if (andmap subtype argtypes new-doms*)
                                 (ret (subst-all substitution (car rngs*)))
                                 ;; FIXME
                                 ;; should be an error here, something went horribly wrong!!!
                                 (begin 
				   #;
                                   (printf "substitution was bad~n args: ~a ~n new-doms: ~a~n~a~n" argtypes new-doms* substitution)
                                   (loop (cdr doms*) (cdr rngs*)))))))]
                             #|
                           (printf "subst is:~a~nret is: ~a~nvars is: ~a~nresult is:~a~n" substitution (car rngs*) vars 
                                   (subst-all substitution (car rngs*)))
                           (printf "new-doms*: ~a~n" new-doms*)
                           (printf "orig doms* is: ~a~n" (car doms*))
                           (printf "argtypes: ~a~n" argtypes)
                           (int-err "Inconsistent substitution - arguments not subtypes")))
                       #;(printf "subst is:~a~nret is: ~a~nvars is: ~a~n" substitution (car rngs*) vars)
                       )]|#
                 [else (loop (cdr doms*) (cdr rngs*))]))]
        ;; polymorphic varargs
        [(tc-result: (Poly: vars (Function: (list (arr: dom rng rest thn-eff els-eff)))))
         (for-each (lambda (x) (unless (not (Poly? x))                                      
                                 (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                   argtypes)
         (unless (<= (length dom) (length argtypes))
           (tc-error "incorrect number of arguments to function: ~a ~a" dom argtypes))
         (let ([substitution 
                (infer/vararg vars argtypes dom rest rng expected)])
           (cond 
             [(and expected substitution) expected]
             [substitution
              (let* ([s (lambda (t) (subst-all substitution t))]
                     [new-dom (map s dom)]
                     [new-rest (s rest)])                                  
                (unless (subtypes/varargs argtypes new-dom new-rest)
                  (int-err "Inconsistent substitution - arguments not subtypes"))
                (ret (subst-all substitution rng)))]
             [else (tc-error/expr #:return (ret (Un))
                                  "no polymorphic function domain matched - domain was: ~a rest type was: ~a arguments were ~a"
                                  (stringify dom) rest (stringify argtypes))]))]
        [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests thn-effs els-effs) ...))))
         (tc-error/expr #:return (ret (Un)) "polymorphic vararg case-lambda application not yet supported")]
        ;; Union of function types works if we can apply all of them
        [(tc-result: (Union: (list (and fs (Function: _)) ...)) e1 e2)
         (match-let ([(list (tc-result: ts) ...) (map (lambda (f) (outer-loop 
                                                                   (ret f e1 e2) argtypes arg-thn-effs arg-els-effs args)) fs)])
           (ret (apply Un ts)))]
        [(tc-result: f-ty _ _) (tc-error/expr #:return (ret (Un)) "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))))

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


(define (tc/app/internal form expected)
  (kernel-syntax-case* form #f 
    (values apply not list list* call-with-values do-make-object make-object cons) ;; the special-cased functions   
    ;; special cases for classes
    [(#%plain-app make-object cl args ...)
     (tc/app/internal #'(#%plain-app do-make-object cl (#%plain-app list args ...) (#%plain-app list)) expected)]     
    [(#%plain-app do-make-object cl (#%plain-app list pos-args ...) (#%plain-app list (#%plain-app cons 'names named-args) ...))
     (let* ([names (map syntax-e (syntax->list #'(names ...)))]
            [name-assoc (map list names (syntax->list #'(named-args ...)))])
       (let loop ([t (tc-expr #'cl)])
         (match t
           [(tc-result: (? Mu? t)) (loop (ret (unfold t)))]
           [(tc-result: (and c (Class: pos-tys (list (and tnflds (list tnames _ _)) ...) _))) 
            (unless (= (length pos-tys)
                       (length (syntax->list #'(pos-args ...))))
              (tc-error "expected ~a positional arguments, but got ~a" (length pos-tys) (length (syntax->list #'(pos-args ...)))))
            (for-each tc-expr/check (syntax->list #'(pos-args ...)) pos-tys)
            (for-each (lambda (n) (unless (memq n tnames)
                                    (tc-error "unknown named argument ~a for class~nlegal named arguments are ~a" n (stringify tnames))))
                      names)
            (for-each (match-lambda
                        [(list tname tfty opt?)
                         (let ([s (cond [(assq tname name-assoc) => cadr]
                                        [(not opt?)
                                         (tc-error "value not provided for named init arg ~a" tname)]
                                        [else #f])])
                           (if s
                               ;; this argument was present
                               (tc-expr/check s tfty)
                               ;; this argument wasn't provided, and was optional
                               #f))])
                      tnflds)
            (ret (make-Instance c))]
           [(tc-result: t)
            (tc-error "expected a class value for object creation, got: ~a" t)])))]
    [(#%plain-app do-make-object . args)
     (int-err "bad do-make-object : ~a" (syntax->datum #'args))]
    ;; call-with-values
    [(#%plain-app call-with-values prod con)
     (match-let* ([(tc-result: prod-t) (tc-expr #'prod)]
                  [(tc-result: con-t) (tc-expr #'con)])
       (match (list prod-t con-t)
         [(list (Function: (list (arr: (list) vals #f _ _))) (Function: (list (arr: dom rng #f _ _))))
          (=> unmatch)
          (match (list vals dom)
            [(list (Values: v) (list t ...))
             (if (subtypes v t)
                 (ret rng)
                 (unmatch))]
            [(list t1 (list t2))
             (if (subtype t1 t2) (ret rng) (unmatch))]
            [_ (unmatch)])]
         [_ (tc-error "Incorrect arguments to call with values: ~a ~a" prod-t con-t)]))]
    ;; special cases for `values'
    [(#%plain-app values arg) (tc-expr #'arg)]
    [(#%plain-app values . args)
     (let ([tys (map tc-expr/t (syntax->list #'args))])
       (ret (list->values-ty tys)))]
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
    [(#%plain-app 
      (letrec-values 
          ([(lp) (#%plain-lambda (val acc ...)
                   (if (#%plain-app null? val*)
                       thn
                       els))])
        lp*)
      actual actuals ...)
     (and ;(printf "got here 0:~a~n" (syntax->datum #'body))
          expected
          ;(printf "got here 1~n")
          (not (andmap type-annotation (syntax->list #'(val acc ...))))
          (free-identifier=? #'val #'val*)
          (ormap (lambda (a) (find-annotation #'(if (#%plain-app null? val*) thn els) a))
                  (syntax->list #'(acc ...)))
          ;(printf "got here 2~n")
          #;
          (match (matches? #'lv-bindings)
            [(list res acc* more)
             (and
              (andmap type-annotation res)
              (andmap free-identifier=? (syntax->list #'(acc ...)) acc*)
              (free-identifier=? #'lp #'lp*))]
            [_ #f]))
     (let* ([ts1 (tc-expr/t #'actual)]
            [ts1 (generalize ts1)]
            [ann-ts (map (lambda (a ac) (or (find-annotation #'(if (#%plain-app null? val*) thn els) a)
                                            (generalize (tc-expr/t ac))))
                         (syntax->list #'(acc ...))
                         (syntax->list #'(actuals ...)))]
            [ts (cons ts1 ann-ts)]) 
       ;(printf "doing match case actuals:~a ann-ts: ~a~n" (syntax->datum #'(actuals ...)) ann-ts)
       ;; check that the actual arguments are ok here
       (map tc-expr/check (syntax->list #'(actuals ...)) ann-ts)
       ;(printf "done here ts = ~a~n" ts)
       ;; then check that the function typechecks with the inferred types
       (tc/rec-lambda/check form
                            #'(val acc ...)
                            #'((if (#%plain-app null? val*)
                                   thn
                                   els))
                            #'lp
                            ts
                            expected)
       (ret expected))]
    ;; special case when argument needs inference
    [(#%plain-app (letrec-values ([(lp) (#%plain-lambda (args ...) . body)]) lp*) . actuals)
     (and ;(printf "special case 0 ~a~n" expected)
          expected
          ;(printf "special case 1~n")
          (not (andmap type-annotation (syntax->list #'(args ...))))
          (free-identifier=? #'lp #'lp*))
     (let ([ts (map (compose generalize tc-expr/t) (syntax->list #'actuals))])
       ;(printf "special case~n")
       (tc/rec-lambda/check form #'(args ...) #'body #'lp ts expected)
       (ret expected))]
    ;; default case
    [(#%plain-app f args ...) 
     (begin
       ;(printf "default case~n~a~n" (syntax->datum form))
       (tc/funapp #'f #'(args ...) (tc-expr #'f) (map tc-expr (syntax->list #'(args ...))) expected))]))
  
;(trace tc/app/internal)
