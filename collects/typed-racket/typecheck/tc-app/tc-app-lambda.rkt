#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match racket/list
         syntax/parse/experimental/reflect
         unstable/sequence
         (typecheck signatures tc-funapp check-below find-annotation )
         (types abbrev utils generalize type-table)
         (private type-annotation)
         (rep type-rep)

         (for-template racket/base))


(import tc-expr^ tc-let^ tc-lambda^)
(export tc-app-lambda^)

(define-tc/app-syntax-class (tc/app-lambda expected)
  #:literals (#%plain-app #%plain-lambda letrec-values)
  ;; let loop
  (pattern (~and form ((letrec-values ([(lp) (~and lam (#%plain-lambda args . body))]) lp*) . actuals))
    #:fail-unless expected #f
    #:fail-unless (not (andmap type-annotation (syntax->list #'(lp . args)))) #f
    #:fail-unless (free-identifier=? #'lp #'lp*) #f
    (let-loop-check #'(#%plain-app . form) #'lam #'lp #'actuals #'args #'body expected))
  ;; inference for ((lambda
  (pattern ((#%plain-lambda (x ...) . body) args ...)
   #:fail-unless (= (length (syntax->list #'(x ...)))
                    (length (syntax->list #'(args ...)))) #f
   #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
   (tc/let-values #'((x) ...) #'(args ...) #'body
                  #'(let-values ([(x) args] ...) . body)
                  expected))
  ;; inference for ((lambda with dotted rest
  (pattern ((#%plain-lambda (x ... . rst:id) . body) args ...)
   #:fail-unless (<= (length (syntax->list #'(x ...)))
                     (length (syntax->list #'(args ...)))) #f
   ;; FIXME - remove this restriction - doesn't work because the annotation
   ;; on rst is not a normal annotation, may have * or ...
   #:fail-when (type-annotation #'rst) #f
   #:fail-when (andmap type-annotation (syntax->list #'(x ...))) #f
   (let-values ([(fixed-args varargs) 
                 (split-at (syntax->list #'(args ...)) (length (syntax->list #'(x ...))))])
     (with-syntax ([(fixed-args ...) fixed-args]
                   [varg #`(#%plain-app list #,@varargs)])
       (tc/let-values #'((x) ... (rst)) #`(fixed-args ... varg) #'body
                      #'(let-values ([(x) fixed-args] ... [(rst) varg]) . body)
                      expected)))))


(define (let-loop-check form lam lp actuals args body expected)
  (syntax-parse #`(#,args #,body #,actuals)
    #:literals (#%plain-app if null? pair? null)
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
                      (or (find-annotation #'inner-body a)
                          (generalize (tc-expr/t ac))))]
            [ts (cons ts1 ann-ts)])
       ;; check that the actual arguments are ok here
       (for/list ([a (syntax->list #'(actuals ...))]
                  [t ann-ts])
         (tc-expr/check a (ret t)))
       ;; then check that the function typechecks with the inferred types
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp ts expected))
       expected)]
    ;; special case `for/list'
    [((val acc ...)
      ((~and inner-body (if e1 e2 e3:id)))
      (null actuals ...))
     #:when (free-identifier=? #'val #'e3)
     (let ([ts (for/list ([ac (syntax->list #'(actuals ...))]
                          [f (syntax->list #'(acc ...))])
                 (or
                  (type-annotation f #:infer #t)
                  (generalize (tc-expr/t ac))))]
           [acc-ty (or
                    (type-annotation #'val #:infer #t)
                    (match expected
                      [(tc-result1: (and t (Listof: _))) t]
                      [_ #f])
                    (generalize (-val '())))])
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp (cons acc-ty ts) expected))
       expected)]
    ;; special case when argument needs inference
    [(_ body* _)
     (let ([ts (for/list ([ac (syntax->list actuals)]
                          [f (syntax->list args)])
                 (let* ([infer-t (or (type-annotation f #:infer #t)
                                     (find-annotation #'(begin . body*) f))])
                   (if infer-t
                       (check-below (tc-expr/t ac) infer-t)
                       (generalize (tc-expr/t ac)))))])
       (add-typeof-expr lam (tc/rec-lambda/check form args body lp ts expected))
       expected)]))

