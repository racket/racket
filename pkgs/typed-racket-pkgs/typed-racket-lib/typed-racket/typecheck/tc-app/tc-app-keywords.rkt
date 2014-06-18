
#lang racket/unit

(require (rename-in "../../utils/utils.rkt" [infer r:infer])
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match racket/set
         (env tvar-env)
         (typecheck signatures tc-app-helper tc-funapp tc-metafunctions)
         (types abbrev utils union substitute subtype)
         (rep type-rep)
         (utils tc-utils)
         (r:infer infer)
         (for-label racket/base))


(import tc-expr^)
(export tc-app-keywords^)

(define-literal-set keyword-literals #:for-label (list))

(define-tc/app-syntax-class (tc/app-keywords expected)
  #:literal-sets (kernel-literals keyword-literals)
  (pattern (~and form
                 ((#%plain-app cpce s-kp fn kpe kws num)
                  kw-list
                  (#%plain-app list . kw-arg-list)
                  . pos-args))
    #:declare cpce (id-from 'checked-procedure-check-and-extract 'racket/private/kw)
    #:declare s-kp (id-from 'struct:keyword-procedure 'racket/private/kw)
    #:declare kpe  (id-from 'keyword-procedure-extract 'racket/private/kw)
    (match (tc-expr/t #'fn)
      [(and ty (Poly: vars (Function: arrs)))
       (define kw-args (type->list (tc-expr/t #'kws)))
       (define kw-arg-tys (stx-map tc-expr/t #'kw-arg-list))
       (define argtys-t (stx-map tc-expr/t #'pos-args))
       (or (for/or ([arr (in-list arrs)])
             (match-define (arr: dom rng rest #f kw-formals) arr)
             ;; get the mapping of the keywords that are both provided and
             ;; in the function type, and hope for the best in inference
             (define-values (kw-actual-tys kw-dom-tys)
               (make-kw-mapping kw-args kw-arg-tys kw-formals))
             (define subst
               (extend-tvars vars
                (infer/vararg vars null
                              (append argtys-t kw-actual-tys)
                              (append dom kw-dom-tys)
                              rest rng
                              (and expected (tc-results->values expected)))))
             (and subst
                  (tc-keywords/internal (subst-all subst arr) kw-args #'kw-arg-list #f)
                  (tc/funapp1 #'form #'pos-args
                              ;; kw checks were already done by tc-keywords/internal
                              ;; so for tc/funapp1 we just give it null for kws
                              (subst-all subst (make-arr dom rng rest #f null))
                              (map ret argtys-t) ; tc/funapp1 expects results
                              expected #:check #f)))
           (poly-fail #'form #'pos-args ty argtys-t
                      #:expected expected))]
      [(Function: arities)
       (tc-keywords #'(#%plain-app . form) arities (type->list (tc-expr/t #'kws))
                    #'kw-arg-list #'pos-args expected)]
      [t
       (tc-error/expr "Cannot apply expression of type ~a, since it is not a function type" t)])))

;; make-kw-mapping : (Listof kw-literal) (Listof Type) (Listof Keyword)
;;                   -> (Listof Type) (Listof Type)
;; Given a list of provided keywords and their argument types, match it up with
;; the keyword types that the function type specifies (if one exists).
(define (make-kw-mapping provided-kws provided-kw-types kw-formals)
  (define kw-formal-map
    (for/hash ([kw-formal kw-formals])
      (values (Keyword-kw kw-formal) (Keyword-ty kw-formal))))
  (for/lists (_1 _2) ([provided-kw provided-kws]
                      [provided-kw-type provided-kw-types]
                      [kw-formal kw-formals]
                      #:when (hash-has-key? kw-formal-map provided-kw))
    (values provided-kw-type (hash-ref kw-formal-map provided-kw))))

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
                (ret (make-Function (list (make-arr* dom rng #:rest rest))))
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
                      (ret (make-Function new-arities))
                      (stx-map tc-expr pos-args) expected)))]))

(define (type->list t)
  (match t
    [(Pair: (Value: (? keyword? k)) b)
     (cons k (type->list b))]
    [(Value: '()) null]
    [_ (int-err "bad value in type->list: ~a" t)]))


