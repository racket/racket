
#lang racket/unit

(require (rename-in "../../utils/utils.rkt" [infer r:infer])
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match racket/set
         (typecheck signatures tc-app-helper tc-funapp tc-metafunctions)
         (types abbrev utils union substitute subtype)
         (rep type-rep)
         (utils tc-utils)
         (r:infer infer)
         (for-label racket/base)
         ;; adjusted -1 since it's provided for-syntax
         (only-in (for-template racket/contract/private/provide)
                  contract-neg-party-property))


(import tc-expr^)
(export tc-app-keywords^)

(define-literal-set keyword-literals #:for-label (list))

(define-tc/app-syntax-class (tc/app-keywords expected)
  #:literal-sets (kernel-literals keyword-literals)
  (pattern (~and form
                 ((#%plain-app cpce s-kp fn kpe kws num)
                  kw-list
                  (#%plain-app list . kw-arg-list)
                  . *pos-args))
    #:declare cpce (id-from 'checked-procedure-check-and-extract 'racket/private/kw)
    #:declare s-kp (id-from 'struct:keyword-procedure 'racket/private/kw)
    #:declare kpe  (id-from 'keyword-procedure-extract 'racket/private/kw)
    ;; If this application is of a module boundary contract function or not
    ;; If #t, then the contract system has inserted an extra argument which we
    ;; need to ignore
    #:attr boundary-ctc? (contract-neg-party-property #'fn)
    #:with pos-args (if (attribute boundary-ctc?)
                        (stx-cdr #'*pos-args)
                        #'*pos-args)
    (match (tc-expr #'fn)
      [(tc-result1:
        (Poly: vars
               (Function: (list (and ar (arr: dom rng (and rest #f) (and drest #f) kw-formals))))))
       (=> fail)
       (unless (set-empty? (fv/list kw-formals))
         (fail))
       (match (stx-map single-value #'pos-args)
         [(list (tc-result1: argtys-t) ...)
          (let* ([subst (infer vars null argtys-t dom rng
                               (and expected (tc-results->values expected)))])
            (unless subst (fail))
            (tc-keywords #'form (list (subst-all subst ar))
                         (type->list (tc-expr/t #'kws)) #'kw-arg-list #'pos-args expected))])]
      [(tc-result1: (Function: arities))
       (tc-keywords #'(#%plain-app . form) arities (type->list (tc-expr/t #'kws))
                    #'kw-arg-list #'pos-args expected)]
      [(tc-result1: (Poly: _ (Function: _)))
       (tc-error/expr "Inference for polymorphic keyword functions not supported")]
      [(tc-result1: t)
       (tc-error/expr "Cannot apply expression of type ~a, since it is not a function type" t)])))

(define (tc-keywords/internal arity kws kw-args error?)
  (match arity
    [(arr: dom rng rest #f ktys)
     ;; assumes that everything is in sorted order
     (let loop ([actual-kws kws]
                [actuals (stx-map tc-expr/t kw-args)]
                [formals ktys])
       (match* (actual-kws formals)
         [('() '())
          (void)]
         [(_ '())
          (if error?
              (tc-error/delayed "Unexpected keyword argument ~a" (car actual-kws))
              #f)]
         [('() (cons fst rst))
          (match fst
            [(Keyword: k _ #t)
             (if error?
                 (tc-error/delayed "Missing keyword argument ~a" k)
                 #f)]
            [_ (loop actual-kws actuals rst)])]
         [((cons k kws-rest) (cons (Keyword: k* t req?) form-rest))
          (cond [(eq? k k*) ;; we have a match
                 (if (subtype (car actuals) t)
                     ;; success
                     (loop kws-rest (cdr actuals) form-rest)
                     ;; failure
                     (and error?
                          (tc-error/delayed
                           "Wrong function argument type, expected ~a, got ~a for keyword argument ~a"
                           t (car actuals) k)
                          (loop kws-rest (cdr actuals) form-rest)))]
                [req? ;; this keyword argument was required
                 (if error?
                     (begin (tc-error/delayed "Missing keyword argument ~a" k*)
                            (loop kws-rest (cdr actuals) form-rest))
                     #f)]
                [else ;; otherwise, ignore this formal param, and continue
                 (loop actual-kws actuals form-rest)])]))]))

(define (tc-keywords form arities kws kw-args pos-args expected)
  (match arities
    [(list (and a (arr: dom rng rest #f ktys)))
     (tc-keywords/internal a kws kw-args #t)
     (tc/funapp (car (syntax-e form)) kw-args
                (->* dom rest rng)
                (stx-map tc-expr pos-args) expected)]
    [(list (and a (arr: doms rngs rests (and drests #f) ktyss)) ...)
     (let ([new-arities
            (for/list ([a (in-list arities)]
                       ;; find all the arities where the keywords match
                       #:when (tc-keywords/internal a kws kw-args #f))
              (match a
                [(arr: dom rng rest #f ktys) (make-arr* dom rng #:rest rest)]))])
       (if (null? new-arities)
           (domain-mismatches
            (car (syntax-e form)) (cdr (syntax-e form))
            arities doms rests drests rngs
            (stx-map tc-expr pos-args)
            #f #f #:expected expected
            #:msg-thunk
            (lambda (dom)
              (string-append "No function domains matched in function application:\n"
                             dom)))
           (tc/funapp (car (syntax-e form)) kw-args
                      (make-Function new-arities)
                      (stx-map tc-expr pos-args) expected)))]))

(define (type->list t)
  (match t
    [(Pair: (Value: (? keyword? k)) b)
     (cons k (type->list b))]
    [(Value: '()) null]
    [_ (int-err "bad value in type->list: ~a" t)]))


