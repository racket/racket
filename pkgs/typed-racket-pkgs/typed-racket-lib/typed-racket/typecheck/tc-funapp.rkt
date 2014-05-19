#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match syntax/stx
         (prefix-in c: (contract-req))
         (env tvar-env)
         (types utils union subtype resolve abbrev
                substitute classes)
         (typecheck tc-metafunctions tc-app-helper check-below)
         (rep type-rep)
         (r:infer infer))

(provide/cond-contract
  [tc/funapp
   (syntax? stx-list? Type/c (c:listof tc-results1/c)
    (c:or/c #f tc-results/c)
    . c:-> . full-tc-results/c)])

(define (tc/funapp f-stx args-stx f-type args-res expected)
  (match-define (list (tc-result1: argtys) ...) args-res)
  (match f-type
    ;; we special-case this (no case-lambda) for improved error messages
    ;; tc/funapp1 currently cannot handle drest arities
    [(Function: (list (and a (arr: _ _ _ #f _))))
     (tc/funapp1 f-stx args-stx a args-res expected)]
    ;; Standard function case.
    [(AnyPoly: fixed-vars dotted-vars (Function/arrs: doms rngs rests drests kwss #:arrs arrs))
     (or (for/or ([dom (in-list doms)]
                  [rng (in-list rngs)]
                  [rest (in-list rests)]
                  [drest (in-list drests)]
                  [kws (in-list kwss)]
                  [a (in-list arrs)]
                  #:unless (ormap Keyword-required? kws))
           (extend-tvars fixed-vars
             (define substitution
               (infer fixed-vars dotted-vars
                      (list (-Tuple argtys))
                      (list (-Tuple* dom
                              (cond
                                [rest (-lst rest)]
                                [drest (make-ListDots (car drest) (cdr drest))]
                                [else -Null])))
                      rng
                      (and expected (tc-results->values expected))))
             (and substitution
                  (tc/funapp1 f-stx args-stx (subst-all substitution a)
                              args-res expected #:check #f))))
         (if (and (null? fixed-vars) (null? dotted-vars))
             (domain-mismatches
               f-stx args-stx f-type doms rests drests rngs args-res #f #f
               #:expected expected
               #:msg-thunk (lambda (dom)
                             (string-append
                              "No function domains matched in function application:\n"
                              dom)))
             (poly-fail f-stx args-stx f-type args-res
                        #:name (and (identifier? f-stx) f-stx)
                        #:expected expected)))]
    ;; Row polymorphism. For now we do really dumb inference that only works
    ;; in very restricted cases, but is probably enough for most cases in
    ;; the Racket codebase. Eventually this should be extended.
    [(PolyRow: vars constraints (and f-ty (Function/arrs: doms _ _ #f _)))
     (define (fail)
       (poly-fail f-stx args-stx f-type args-res
                  #:name (and (identifier? f-stx) f-stx)
                  #:expected expected))
     ;; there's only one row variable in a PolyRow (for now)
     (define row-var (car vars))
     ;; only infer if there is only one argument type that has the relevant
     ;; row type variable in its free variables in all cases
     (define row-var-idxs
       (for/list ([dom doms])
         (define num-occurs
           (for/list ([dom-type dom] [idx (in-naturals)]
                      #:when (member row-var (fv dom-type)))
             idx))
         (unless (<= (length num-occurs) 1)
           (fail))
         (if (null? num-occurs) 0 (car num-occurs))))
     (unless (or (< (length row-var-idxs) 2)
                 (apply = row-var-idxs))
       ;; row var wasn't in the same position in some cases
       (fail))
     (define idx (car row-var-idxs))
     (define resolved-argty (resolve (list-ref argtys idx)))
     (cond [(Class? resolved-argty)
            (define substitution
              (hash row-var
                    (t-subst (infer-row constraints resolved-argty))))
            (tc/funapp f-stx args-stx (subst-all substitution f-ty)
                       args-res expected)]
           [else (fail)])]
    ;; procedural structs
    [(Struct: _ _ _ (? Function? proc-ty) _ _)
     (tc/funapp f-stx #`(#,(syntax/loc f-stx dummy) . #,args-stx) proc-ty
                (cons (ret f-type) args-res) expected)]
    ;; parameters are functions too
    [(Param: in out)
     (match argtys
      [(list) (ret out)]
      [(list t)
       (if (subtype t in)
           (ret -Void -true-filter)
           (tc-error/expr
            #:return (ret -Void -true-filter)
            "Wrong argument to parameter - expected ~a and got ~a"
            in t))]
      [_ (tc-error/expr
           "Wrong number of arguments to parameter - expected 0 or 1, got ~a"
           (length argtys))])]
    ;; resolve names, polymorphic apps, mu, etc
    [(? needs-resolving?)
     (tc/funapp f-stx args-stx (resolve-once f-type) args-res expected)]
    ;; a union of functions can be applied if we can apply all of the elements
    [(Union: (and ts (list (? Function?) ...)))
     (merge-tc-results
      (for/list ([fty ts])
        (tc/funapp f-stx args-stx fty argtys expected)))]
    ;; error type is a perfectly good fcn type
    [(Error:) f-type]
    ;; otherwise fail
    [(Poly: ns (Function: arrs))
     (tc-error/expr
      (string-append "Cannot infer type instantiation for type ~a. Please add "
                     "more type annotations")
      f-type)]
    [_
     (tc-error/expr
      "Cannot apply expression of type ~a, since it is not a function type"
      f-type)]))
