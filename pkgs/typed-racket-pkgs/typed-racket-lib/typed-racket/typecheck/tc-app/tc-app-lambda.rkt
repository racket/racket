#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         (prefix-in - (contract-req))
         syntax/parse racket/match racket/list
         unstable/sequence unstable/syntax
         (typecheck signatures find-annotation)
         (types abbrev utils generalize type-table)
         (private type-annotation syntax-properties)
         ;; Needed to construct args to tc/let-values
         (for-template racket/base)
         (for-label racket/base))


(import tc-expr^ tc-let^ tc-lambda^)
(export tc-app-lambda^)

(define-literal-set lambda-literals
  #:for-label
  (null? pair? null))

(define-tc/app-syntax-class (tc/app-lambda expected)
  #:literal-sets (kernel-literals)
  ;; let loop
  (pattern ((letrec-values ([(lp) (~and lam (#%plain-lambda (args ...) . body))]) lp*) . actuals)
    #:when expected
    #:when (not (andmap type-annotation (syntax->list #'(lp args ...))))
    #:when (free-identifier=? #'lp #'lp*)
    (let-loop-check #'lam #'lp #'actuals #'(args ...) #'body expected))
  ;; inference for ((lambda
  (pattern ((~and lam (#%plain-lambda (x ...) . body)) args ...)
   #:fail-when (plambda-property #'lam) #f
   #:fail-unless (= (syntax-length #'(x ...))
                    (syntax-length #'(args ...))) #f
   #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
   (tc/let-values #'((x) ...) #'(args ...) #'body expected))
  ;; inference for ((lambda with dotted rest
  (pattern ((~and lam (#%plain-lambda (x ... . rst:id) . body)) args ...)
   #:fail-when (plambda-property #'lam) #f
   #:fail-unless (<= (syntax-length #'(x ...))
                     (syntax-length #'(args ...))) #f
   ;; FIXME - remove this restriction - doesn't work because the annotation
   ;; on rst is not a normal annotation, may have * or ...
   #:fail-when (type-annotation #'rst) #f
   #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
   (let-values ([(fixed-args varargs) 
                 (split-at (syntax->list #'(args ...)) (syntax-length #'(x ...)))])
     (with-syntax ([(fixed-args ...) fixed-args]
                   [varg #`(#%plain-app list #,@varargs)])
       (tc/let-values #'((x) ... (rst)) #`(fixed-args ... varg) #'body
                      expected)))))


(define/cond-contract
  (let-loop-check lam lp actuals args body expected)
  (syntax? syntax? syntax? syntax? syntax? tc-results/c . --> . full-tc-results/c)
  (syntax-parse #`(#,args #,body #,actuals)
    #:literal-sets (kernel-literals lambda-literals)
    [((val acc ...)
      ((~and inner-body (if (#%plain-app (~or pair? null?) val*) thn els)))
      (actual actuals ...))
     #:when
     (and (free-identifier=? #'val #'val*)
          (ormap (lambda (a) (find-annotation #'inner-body a))
                 (syntax->list #'(acc ...))))
     (let* ([ts1 (generalize (tc-expr/t #'actual))]
            [ann-ts (for/list ([a (in-syntax #'(acc ...))]
                               [ac (in-syntax #'(actuals ...))])
                      (let ([type (find-annotation #'inner-body a)])
                        (if type
                            (tc-expr/check/t ac (ret type))
                            (generalize (tc-expr/t ac)))))]
            [ts (cons ts1 ann-ts)])
       ;; check that the actual arguments are ok here
       (for/list ([a (in-syntax #'(actuals ...))]
                  [t (in-list ann-ts)])
         (tc-expr/check a (ret t)))
       ;; then check that the function typechecks with the inferred types
       (define-values (fun-results body-results)
         (tc/rec-lambda/check args body lp ts expected))
       (add-typeof-expr lam fun-results)
       body-results)]
    ;; special case `for/list'
    [((val acc ...)
      ((~and inner-body (if e1 e2 e3:id)))
      (~and (null actuals ...) (null-exp . _)))
     #:when (free-identifier=? #'val #'e3)
     (let ([ts (for/list ([ac (in-syntax #'(actuals ...))]
                          [f (in-syntax #'(acc ...))])
                 (let ([type (type-annotation f #:infer #t)])
                   (if type
                       (tc-expr/check/t ac (ret type))
                       (generalize (tc-expr/t ac)))))]
           [acc-ty (or
                    (type-annotation #'val #:infer #t)
                    (match expected
                      [(tc-result1: (and t (Listof: _))) t]
                      [_ #f])
                    (generalize -Null))])
       ;; this check is needed because the type annotation may come
       ;; from `for/fold` and it won't necessarily be a list type
       (tc-expr/check #'null-exp (ret acc-ty))
       (define-values (fun-results body-results)
         (tc/rec-lambda/check args body lp (cons acc-ty ts) expected))
       (add-typeof-expr lam fun-results)
       body-results)]
    ;; special case when argument needs inference
    [(_ body* _)
     (let ([ts (for/list ([ac (in-syntax actuals)]
                          [f (in-syntax args)])
                 (let* ([infer-t (or (type-annotation f #:infer #t)
                                     (find-annotation #'(begin . body*) f))])
                   (if infer-t
                       (tc-expr/check/t ac (ret infer-t))
                       (generalize (tc-expr/t ac)))))])
       (define-values (fun-results body-results)
         (tc/rec-lambda/check args body lp ts expected))
       (add-typeof-expr lam fun-results)
       body-results)]))

