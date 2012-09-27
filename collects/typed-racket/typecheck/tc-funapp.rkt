#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-app-helper.rkt" "find-annotation.rkt"
         (prefix-in c: racket/contract)
         syntax/parse racket/match racket/list
         ;; fixme - don't need to be bound in this phase - only to make
         ;; syntax/parse happy
         racket/bool racket/unsafe/ops
         (only-in racket/private/class-internal make-object do-make-object)
         (only-in '#%kernel [apply k:apply])
         ;; end fixme
         (for-syntax syntax/parse racket/base (utils tc-utils))
         (private type-annotation)
         (types utils union subtype resolve abbrev type-table substitute)
         (utils tc-utils)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep rep-utils)
         (r:infer infer)
         '#%paramz
         (for-template
          racket/unsafe/ops
          (only-in '#%kernel [apply k:apply])
          "internal-forms.rkt" racket/base racket/bool '#%paramz
          (only-in racket/private/class-internal make-object do-make-object)))

(provide tc/funapp)

(define-syntax (handle-clauses stx)
  (syntax-parse stx
    [(_  (lsts ... arrs) f-stx args-stx pred infer t argtys expected)
     (with-syntax ([(vars ... a) (generate-temporaries #'(lsts ... arrs))])
       (syntax/loc stx
         (or (for/or ([vars lsts] ... [a arrs]
                      #:when (pred vars ... a))
               (let ([substitution (infer vars ... a)])
                 (and substitution
                      (tc/funapp1 f-stx args-stx (subst-all substitution a)
                                  argtys expected #:check #f))))
             (poly-fail f-stx args-stx t argtys
                        #:name (and (identifier? f-stx) f-stx)
                        #:expected expected))))]))

(define/cond-contract (tc/funapp f-stx args-stx ftype0 argtys expected)
  (syntax? (c:and/c syntax? syntax->list) tc-results? (c:listof tc-results?)
           (c:or/c #f tc-results?)
           . c:-> . tc-results?)
  (match* (ftype0 argtys)
    ;; we special-case this (no case-lambda) for improved error messages
    [((tc-result1: (and t (Function: (list (and a (arr: dom (Values: _)
                                                        rest #f kws))))))
      argtys)
     (tc/funapp1 f-stx args-stx a argtys expected)]
    [((tc-result1: (and t (Function: (and arrs (list (arr: doms rngs rests
                                                           (and drests #f) kws)
                                                     ...)))))
      (and argtys (list (tc-result1: argtys-t) ...)))
     (or
      ;; find the first function where the argument types match
      (for/first ([dom (in-list doms)] [rng (in-list rngs)] [rest (in-list rests)] [a (in-list arrs)]
                  #:when (subtypes/varargs argtys-t dom rest))
        ;; then typecheck here
        ;; we call the separate function so that we get the appropriate
        ;; filters/objects
        (tc/funapp1 f-stx args-stx a argtys expected #:check #f))
      ;; if nothing matched, error
      (domain-mismatches
       f-stx args-stx t doms rests drests rngs argtys #f #f
       #:expected expected #:return (or expected (ret (Un)))
       #:msg-thunk (lambda (dom)
                     (string-append
                      "No function domains matched in function application:\n"
                      dom))))]
    ;; any kind of dotted polymorphic function without mandatory keyword args
    [((tc-result1:
       (and t (PolyDots:
               (and vars (list fixed-vars ... dotted-var))
               (Function: (list (and arrs (arr: doms rngs rests drests
                                                (list (Keyword: _ _ #f) ...)))
                                ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses
      (doms rngs rests drests arrs) f-stx args-stx
      ;; only try inference if the argument lengths are appropriate
      (lambda (dom _ rest drest a)
        (cond [rest (<= (length dom) (length argtys))]
              [drest (and (<= (length dom) (length argtys))
                          (eq? dotted-var (cdr drest)))]
              [else (= (length dom) (length argtys))]))
      ;; Only try to infer the free vars of the rng (which includes the vars
      ;; in filters/objects). Note that we have to use argtys-t here, since
      ;; argtys is a list of tc-results.
      (λ (dom rng rest drest a)
        (cond
         [drest
          (infer/dots
           fixed-vars dotted-var argtys-t dom (car drest) rng (fv rng)
           #:expected (and expected (tc-results->values expected)))]
         [rest
          (infer/vararg fixed-vars (list dotted-var) argtys-t dom rest rng
                        (and expected (tc-results->values expected)))]
         ;; no rest or drest
         [else (infer fixed-vars (list dotted-var) argtys-t dom rng
                      (and expected (tc-results->values expected)))]))
      t argtys expected)]
    ;; regular polymorphic functions without dotted rest, 
    ;; we do not choose any instantiations with mandatory keyword arguments
    [((tc-result1:
       (and t (Poly:
               vars
               (Function: (list (and arrs (arr: doms rngs rests (and drests #f)
                                                (list (Keyword: _ _ kw?) ...)))
                                ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses
      (doms rngs rests kw? arrs) f-stx args-stx
      ;; only try inference if the argument lengths are appropriate
      ;; and there's no mandatory kw
      (λ (dom _ rest kw? a) 
        (and (andmap not kw?) ((if rest <= =) (length dom) (length argtys))))
      ;; Only try to infer the free vars of the rng (which includes the vars
      ;; in filters/objects). Note that we have to use argtys-t here, since
      ;; argtys is a list of tc-results.
      (λ (dom rng rest kw? a)
         (infer/vararg vars null argtys-t dom rest rng
                       (and expected (tc-results->values expected))))
      t argtys expected)]
    ;; procedural structs
    [((tc-result1: (and sty (Struct: _ _ _ (? Function? proc-ty) _ _))) _)
     (tc/funapp f-stx #`(#,(syntax/loc f-stx dummy) . #,args-stx) (ret proc-ty)
                (cons ftype0 argtys) expected)]
    ;; parameters are functions too
    [((tc-result1: (Param: in out)) (list)) (ret out)]
    [((tc-result1: (Param: in out)) (list (tc-result1: t)))
     (if (subtype t in)
         (ret -Void true-filter)
         (tc-error/expr
          #:return (ret -Void true-filter)
          "Wrong argument to parameter - expected ~a and got ~a"
          in t))]
    [((tc-result1: (Param: _ _)) _)
     (tc-error/expr
      #:return (ret (Un))
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
    [((tc-result1: (and f-ty (Poly: ns (Function: arrs)))) _)
     (tc-error/expr
      #:return (ret (Un))
      (string-append "Cannot infer type instantiation for type ~a. Please add "
                     "more type annotations")
      f-ty)]
    [((tc-result1: f-ty) _)
     (tc-error/expr
      #:return (ret (Un))
      "Cannot apply expression of type ~a, since it is not a function type"
      f-ty)]))
