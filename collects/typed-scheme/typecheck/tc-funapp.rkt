#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-app-helper.rkt" "find-annotation.rkt"
         "tc-subst.rkt" "check-below.rkt"
         (prefix-in c: racket/contract)
         syntax/parse racket/match racket/list 
	 unstable/sequence unstable/debug
         ;; fixme - don't need to be bound in this phase - only to make syntax/parse happy
         racket/bool
         racket/unsafe/ops
         (only-in racket/private/class-internal make-object do-make-object)
         (only-in '#%kernel [apply k:apply])
         ;; end fixme
         (for-syntax syntax/parse racket/base (utils tc-utils))
         (private type-annotation)
         (types utils abbrev union subtype resolve convenience type-table substitute)
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
                      (tc/funapp1 f-stx args-stx (subst-all substitution a) argtys expected #:check #f))))
             (poly-fail t argtys #:name (and (identifier? f-stx) f-stx) #:expected expected))))]))

(d/c (tc/funapp f-stx args-stx ftype0 argtys expected)
  (syntax? syntax? tc-results? (c:listof tc-results?) (c:or/c #f tc-results?) . c:-> . tc-results?)
  (match* (ftype0 argtys)
    ;; we special-case this (no case-lambda) for improved error messages
    [((tc-result1: (and t (Function: (list (and a (arr: dom (Values: _) rest #f kws)))))) argtys)
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
    ;; any kind of dotted polymorphic function without mandatory keyword args
    [((tc-result1: (and t (PolyDots: 
                           (and vars (list fixed-vars ... dotted-var))
                           (Function: (list (and arrs (arr: doms rngs rests drests (list (Keyword: _ _ #f) ...))) ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms rngs rests drests arrs) f-stx args-stx
                     ;; only try inference if the argument lengths are appropriate
                     (lambda (dom _ rest drest a) 
                       (cond [rest (<= (length dom) (length argtys))]
                             [drest (and (<= (length dom) (length argtys))
                                         (eq? dotted-var (cdr drest)))]
                             [else (= (length dom) (length argtys))]))
                     ;; only try to infer the free vars of the rng (which includes the vars in filters/objects)
                     ;; note that we have to use argtys-t here, since argtys is a list of tc-results
                     (lambda (dom rng rest drest a)
                       (cond 
                         [drest
                          (infer/dots fixed-vars dotted-var argtys-t dom (car drest) rng (fv rng) 
                                       #:expected (and expected (tc-results->values expected)))]
                         [rest 
                          (infer/vararg fixed-vars (list dotted-var) argtys-t dom rest rng
                                        (and expected (tc-results->values expected)))]
                         ;; no rest or drest
                         [else (infer fixed-vars (list dotted-var) argtys-t dom rng
                                      (and expected (tc-results->values expected)))]))
                     t argtys expected)]
    ;; regular polymorphic functions without dotted rest, and without mandatory keyword args
    [((tc-result1: 
       (and t
            (Poly: 
             vars 
             (Function: (list (and arrs (arr: doms rngs rests (and drests #f) (list (Keyword: _ _ #f) ...))) ...)))))
      (list (tc-result1: argtys-t) ...))
     (handle-clauses (doms rngs rests arrs) f-stx args-stx
                     ;; only try inference if the argument lengths are appropriate
                     (λ (dom _ rest a) ((if rest <= =) (length dom) (length argtys)))
                     ;; only try to infer the free vars of the rng (which includes the vars in filters/objects)
                     ;; note that we have to use argtys-t here, since argtys is a list of tc-results
                     (λ (dom rng rest a) (infer/vararg vars null argtys-t dom rest rng (and expected (tc-results->values expected))))
                     t argtys expected)]
    ;; procedural structs
    [((tc-result1: (and sty (Struct: _ _ _ (? Function? proc-ty) _ _ _ _))) _)
     (tc/funapp f-stx #`(#,(syntax/loc f-stx dummy) . #,args-stx) (ret proc-ty) (cons ftype0 argtys) expected)]
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
    [((tc-result1: f-ty) _) 
     (tc-error/expr #:return (ret (Un))
                    "Cannot apply expression of type ~a, since it is not a function type" f-ty)]))


;; syntax? syntax? arr? (listof tc-results?) (or/c #f tc-results) [boolean?] -> tc-results?
(d/c (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  ((syntax? syntax? arr? (c:listof tc-results?) (c:or/c #f tc-results?)) (#:check boolean?) . c:->* . tc-results?)
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((arr: dom (Values: (and results (list (Result: t-r f-r o-r) ...))) rest #f (and kws (list (Keyword: _ _ #f) ...)))
      (list (tc-result1: t-a phi-a o-a) ...))
     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected ~a and got ~a" (length dom) (length t-a))]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected at least ~a and got ~a" (length dom) (length t-a))])
       (for ([dom-t (if rest (in-sequence-forever dom rest) (in-list dom))] 
             [a (in-list (syntax->list args-stx))]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let* ([dom-count (length dom)]
            [arg-count (+ dom-count (if rest 1 0) (length kws))])
       (let-values
           ([(o-a t-a) (for/lists (os ts)
                         ([nm (in-range arg-count)]
                          [oa (in-sequence-forever (in-list o-a) (make-Empty))]
                          [ta (in-sequence-forever (in-list t-a) (Un))])
                         (values (if (>= nm dom-count) (make-Empty) oa)
                                 ta))])
         (define-values (t-r f-r o-r)
           (for/lists (t-r f-r o-r) 
             ([r (in-list results)])
             (open-Result r o-a t-a)))
         (ret t-r f-r o-r)))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args ~a ~a NYI" drest argtys)]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))
