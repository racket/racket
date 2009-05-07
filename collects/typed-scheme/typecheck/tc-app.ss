#lang scheme/unit

(require (rename-in "../utils/utils.ss" [infer r:infer])
         "signatures.ss" "tc-metafunctions.ss"
         "tc-app-helper.ss"
         stxclass scheme/match mzlib/trace scheme/list
         (for-syntax stxclass scheme/base)
         (types utils abbrev union subtype resolve)
         (utils tc-utils)
         (rep type-rep filter-rep object-rep)
         (r:infer infer)
         (for-template 
          (only-in '#%kernel [apply k:apply])
          "internal-forms.ss" scheme/base 
          (only-in scheme/private/class-internal make-object do-make-object)))

(import tc-expr^ tc-lambda^ tc-dots^ tc-let^)
(export tc-app^)

;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values
                values apply k:apply not list list* call-with-values do-make-object make-object cons
                       andmap ormap)
    ;; special case for `values'
    [(#%plain-app values arg) (single-value #'arg expected)]
    [(#%plain-app values . args)
     (match expected
       [(tc-results: ets efs eos)
        (match-let ([(list (tc-result1: ts fs os) ...) 
                     (for/list ([arg (syntax->list #'args)]
                                [et ets] [ef efs] [eo eos])
                       (single-value arg (ret et ef eo)))])
          (if (= (length ts) (length ets) (length (syntax->list #'args)))
              (ret ts fs os)
              (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                             (length ets) (length (syntax->list #'args)))))]
       [_ (match-let ([(list (tc-result1: ts fs os) ...) 
                       (for/list ([arg (syntax->list #'args)])
                         (single-value arg))])
            (ret ts fs os))])]    
    ;; special case for `delay'
    [(#%plain-app 
      mp1 
      (#%plain-lambda () 
        (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
     #:declare mp1 (id-from 'make-promise 'scheme/promise)
     #:declare mp2 (id-from 'make-promise 'scheme/promise)
     (ret (-Promise (tc-expr/t #'e)))]
    ;; special case for `list'
    [(#%plain-app list . args)
     (let ([tys (map tc-expr/t (syntax->list #'args))])
       (ret (apply -lst* tys)))]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                  [tys (reverse tys-r)])
       (ret (foldr make-Pair last tys)))]
    ;; inference for ((lambda
    [(#%plain-app (#%plain-lambda (x ...) . body) args ...)
     #:when (= (length (syntax->list #'(x ...)))
               (length (syntax->list #'(args ...))))
     (tc/let-values #'((x) ...) #'(args ...) #'body 
                    #'(let-values ([(x) args] ...) . body)
                    expected)]
    ;; inference for ((lambda with dotted rest
    [(#%plain-app (#%plain-lambda (x ... . rst:id) . body) args ...)
     #:when (<= (length (syntax->list #'(x ...)))
                (length (syntax->list #'(args ...))))
     (let-values ([(fixed-args varargs) (split-at (syntax->list #'(args ...)) (length (syntax->list #'(x ...))))])
       (with-syntax ([(fixed-args ...) fixed-args]
                     [varg #`(#%plain-app list #,@varargs)])
         (tc/let-values #'((x) ... (rst)) #`(fixed-args ... varg) #'body 
                        #'(let-values ([(x) fixed-args] ... [(rst) varg]) . body)
                        expected)))]
    [(#%plain-app f . args)
     (let* ([f-ty (single-value #'f)]
            [arg-tys (map single-value (syntax->list #'args))])
       (tc/funapp #'f #'args f-ty arg-tys expected))]))

;(trace tc/app/internal)

;; syntax -> tc-results
(define (tc/app form) (tc/app/internal form #f))  
  
;; syntax tc-results? -> tc-results?
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected)
    expected)

(define (object-index os i)
  (unless (number? i)
    (int-err "object-index for keywords NYI"))
  (list-ref os i))

;; in-indexes : Listof[Type] -> Sequence[index/c]
(define (in-indexes dom)
  (in-range (length dom)))


(define-syntax (handle-clauses stx)
  (syntax-parse stx
    [(_  (lsts ... arrs) f-stx args-stx pred infer t argtys expected)
     (with-syntax ([(vars ... a) (generate-temporaries #'(lsts ... arrs))])
       (syntax/loc stx
         (or (for/or ([vars lsts] ... [a arrs]
                      #:when (pred vars ... a))
               (let ([substitution (infer vars ... a)])
                 (and substitution
                      (tc/funapp1 f-stx args-stx (subst-all substitution a) argtys expected #:check #f))))
             (poly-fail t argtys #:name (and (identifier? f-stx) f-stx) #:expected expected))))]))

(define (tc/funapp f-stx args-stx ftype0 argtys expected)
  (match* (ftype0 argtys)
    ;; we special-case this (no case-lambda) for improved error messages
    [((tc-result1: (and t (Function: (list (and a (arr: dom (Values: (list (Result: t-r lf-r lo-r) ...)) rest #f kws))))))
      argtys)
     (tc/funapp1 f-stx args-stx a argtys expected)]
    [((tc-result1: (and t (Function: (and arrs (list (arr: doms rngs rests (and drests #f) kws) ...)))))
      (and argtys (list (tc-result1: argtys-t) ...)))
     (or 
      ;; find the first function where the argument types match
      (for/first ([dom doms] [rng rngs] [rest rests] [a arrs]
                  #:when (subtypes/varargs argtys-t dom rest))
        ;; then typecheck here
        ;; we call the separate function so that we get the appropriate filters/objects
        (tc/funapp1 f-stx args-stx a argtys expected #:check #f))
      ;; if nothing matched, error
      (tc-error/expr 
       #:return (or expected (ret (Un)))
       (string-append "No function domains matched in function application:\n"
                      (domain-mismatches t doms rests drests rngs argtys-t #f #f))))]
    ;; polymorphic functions without dotted rest
    [((tc-result1: (and t
                      (or (Poly: vars 
                                 (Function: (list (and arrs (arr: doms rngs rests (and drests #f) '())) ...)))
                          (PolyDots: vars
                                     (Function: (list (and arrs (arr: doms rngs rests (and drests #f) '())) ...))))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms rngs rests arrs) f-stx args-stx
                     ;; only try inference if the argument lengths are appropriate
                     (lambda (dom _ rest a) ((if rest <= =) (length dom) (length argtys)))
                     ;; only try to infer the free vars of the rng (which includes the vars in filters/objects)
                     ;; note that we have to use argtys-t here, since argtys is a list of tc-results
                     (lambda (dom rng rest a) (infer/vararg vars argtys-t dom rest rng (fv rng) (and expected (tc-results->values expected))))
                     t argtys expected)]
    ;; polymorphic ... type
    [((tc-result1: (and t (PolyDots: 
                         (and vars (list fixed-vars ... dotted-var))
                         (Function: (list (and arrs (arr: doms rngs (and #f rests) (cons dtys dbounds) '())) ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms dtys dbounds rngs arrs) f-stx args-stx
                     (lambda (dom dty dbound rng arr) (and (<= (length dom) (length argtys))
                                                           (eq? dotted-var dbound)))
                     (lambda (dom dty dbound rng arr) 
                       (infer/dots fixed-vars dotted-var argtys-t dom dty rng (fv rng) #:expected (and expected (tc-results->values expected))))
                     t argtys expected)]
    ;; procedural structs
    [(tc-result1: (and sty (Struct: _ _ _ (? Type? proc-ty) _ _ _)))
     (tc/funapp f-stx (cons (syntax/loc f-stx dummy) args-stx) (ret proc-ty) (cons sty argtys) expected)]
    ;; parameters are functions too
    [((tc-result1: (Param: in out)) (list)) (ret out)]
    [((tc-result1: (Param: in out)) (list (tc-result1: t)))
     (if (subtype t in) 
         (ret -Void true-filter)
         (tc-error/expr #:return (ret -Void true-filter)
                        "Wrong argument to parameter - expected ~a and got ~a" in t))]
    [((tc-result1: (Param: _ _)) _) 
     (tc-error/expr #:return (ret (Un))
                    "Wrong number of arguments to parameter - expected 0 or 1, got ~a"
                    (length argtys))]
    ;; resolve names, polymorphic apps, mu, etc
    [((tc-result1: (? needs-resolving? t) f o) _)
     (tc/funapp f-stx args-stx (ret (resolve-once t) f o) argtys expected)]
    ;; a union of functions can be applied if we can apply all of the elements
    [((tc-result1: (Union: (and ts (list (Function: _) ...)))) _)
     (ret (for/fold ([result (Un)]) ([fty ts])            
            (match (tc/funapp f-stx args-stx (ret fty) argtys expected)
              [(tc-result1: t) (Un result t)])))]
    ;; error type is a perfectly good fcn type
    [((tc-result1: (Error:)) _) (ret (make-Error))]
    ;; otherwise fail
    [((tc-result1: f-ty) _) (tc-error/expr #:return (ret (Un))
                                           "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))


;; syntax? syntax? arr? (listof tc-results?) (or/c #f tc-results) [boolean?] -> tc-results?
(define (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  (match* (ftype0 argtys)
    [((arr: dom (Values: (list (Result: t-r lf-r lo-r) ...)) rest #f '())
      (list (tc-result1: t-a phi-a o-a) ...))
     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected ~a and got ~a" (length dom) (length t-a))]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected at least ~a and got ~a" (length dom) (length t-a))])
       (for ([dom-t (in-list-forever dom rest)] [a (syntax->list args-stx)] [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let* (;; Listof[Listof[LFilterSet]]
            [lfs-f (for/list ([lf lf-r])
                     (for/list ([i (in-indexes dom)])
                       (split-lfilters lf i)))]
            ;; Listof[FilterSet]
            [f-r (for/list ([lfs lfs-f])
                   (merge-filter-sets 
                    (for/list ([lf lfs] [t t-a] [o o-a])
                      (apply-filter lf t o))))]
            ;; Listof[Object]
            [o-r (for/list ([lo lo-r])                     
                   (match lo
                     [(LPath: pi* i)
                      (match (object-index o-a i)
                        [(Path: pi x) (make-Path (append pi* pi) x)]
                        [_ (make-Empty)])]
                     [_ (make-Empty)]))])
       (ret t-r f-r o-r))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args NYI")]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args NYI")]))