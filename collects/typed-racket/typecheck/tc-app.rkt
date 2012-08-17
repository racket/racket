#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt" "check-below.rkt"
         "tc-app-helper.rkt" "find-annotation.rkt" "tc-funapp.rkt"
         "tc-subst.rkt" (prefix-in c: racket/contract)
         syntax/parse racket/match racket/list
         unstable/sequence  unstable/list
         ;; fixme - don't need to be bound in this phase - only to make tests work
         racket/bool
         racket/unsafe/ops
         (only-in syntax/location module-name-fixup)
         ;; end fixme
         (for-syntax syntax/parse racket/base (utils tc-utils))
         (private type-annotation)
         (types utils union subtype resolve abbrev
                type-table substitute generalize)
         (utils tc-utils)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep object-rep rep-utils)
         (r:infer infer)
         '#%paramz
         (for-template
          racket/unsafe/ops racket/fixnum racket/flonum
          "internal-forms.rkt" racket/base racket/bool '#%paramz
          
          (only-in syntax/location module-name-fixup)))

(import tc-expr^ tc-lambda^ tc-let^ tc-apply^ tc-app-keywords^
        tc-app-hetero^ tc-app-list^ tc-app-apply^ tc-app-values^
        tc-app-objects^ tc-app-eq^ tc-app-lambda^)
(export tc-app^)


;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (or (tc/app-hetero form expected)
      (tc/app-list form expected)
      (tc/app-apply form expected)
      (tc/app-values form expected)
      (tc/app-keywords form expected)
      (tc/app-objects form expected)
      (tc/app-eq form expected)
      (tc/app-lambda form expected)
  (syntax-parse form
    #:literals (#%plain-app #%plain-lambda letrec-values quote
                not false? list
                module-name-fixup cons
                extend-parameterization)
    ;; bail out immediately if we have one of these
    [(#%plain-app rator:special-op . rands) (tc/app/regular form expected)]
    [(#%plain-app extend-parameterization pmz args ...)
     (let loop ([args (syntax->list #'(args ...))])
       (if (null? args) (ret Univ)
           (let* ([p (car args)]
                  [pt (single-value p)]
                  [v (cadr args)]
                  [vt (single-value v)])
             (match pt
               [(tc-result1: (Param: a b))
                (check-below vt a)
                (loop (cddr args))]
               [(tc-result1: t)
                (tc-error/expr #:return (or expected (ret Univ)) "expected Parameter, but got ~a" t)
                (loop (cddr args))]))))]
    ;; use the additional but normally ignored first argument to make-sequence
    ;; to provide a better instantiation
    [(#%plain-app (~var op (id-from 'make-sequence 'racket/private/for))
                  (~and quo (quote (i:id ...))) arg:expr)
     #:when (andmap type-annotation (syntax->list #'(i ...)))
     (match (single-value #'op)
         [(tc-result1: (and t Poly?))
          (tc-expr/check #'quo (ret Univ))
          (tc/funapp #'op #'(quo arg)
                     (ret (instantiate-poly t (extend (list Univ Univ)
                                                      (map type-annotation (syntax->list #'(i ...)))
                                                      Univ)))
                     (list (ret Univ) (single-value #'arg))
                     expected)])]
    ;; special-case for not - flip the filters
    [(#%plain-app (~or false? not) arg)
     (match (single-value #'arg)
       [(tc-result1: t (FilterSet: f+ f-) _)
        (ret -Boolean (make-FilterSet f- f+))])]
    ;; special case for (current-contract-region)'s default expansion
    ;; just let it through without any typechecking, since module-name-fixup
    ;; is a private function from syntax/location, so this must have been
    ;; (quote-module-name) originally.
    [(#%plain-app module-name-fixup src path)
     (ret Univ)]
    ;; special case for `delay'
    [(#%plain-app
      mp1
      (#%plain-lambda ()
        (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
     #:declare mp1 (id-from 'make-promise 'racket/promise)
     #:declare mp2 (id-from 'make-promise 'racket/promise)
     (ret (-Promise (tc-expr/t #'e)))]
    [_ (tc/app/regular form expected)])))

(define (tc/app/regular form expected)
  (syntax-parse form #:literals (#%plain-app)
    [(#%plain-app f . args)
     (let* ([f-ty (single-value #'f)])
       (match f-ty
         [(tc-result1:
           (and t (Function:
                   (list (and a (arr: (? (lambda (d)
                                           (= (length d)
                                              (length (syntax->list #'args))))
                                         dom)
                                      (Values: (list (Result: v (FilterSet: (Top:) (Top:)) (Empty:))))
                                      #f #f (list (Keyword: _ _ #f) ...)))))))
          ;(printf "f dom: ~a ~a\n" (syntax->datum #'f) dom)
          (let ([arg-tys (map (lambda (a t) (tc-expr/check a (ret t)))
                              (syntax->list #'args)
                              dom)])
            (tc/funapp #'f #'args f-ty arg-tys expected))]
         [_
          (let ([arg-tys (map single-value (syntax->list #'args))])
            (tc/funapp #'f #'args f-ty arg-tys expected))]))]))

;(trace tc/app/internal)

;; syntax -> tc-results
(define (tc/app form) (tc/app/internal form #f))

;; syntax tc-results? -> tc-results?
(define (tc/app/check form expected)
    (define t (tc/app/internal form expected))
    (check-below t expected))
