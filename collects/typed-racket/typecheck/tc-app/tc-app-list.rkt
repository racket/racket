#lang racket/unit


(require "../../utils/utils.rkt"
         syntax/parse racket/match
         (only-in '#%kernel [reverse k:reverse])
         (typecheck signatures tc-app-helper tc-funapp check-below)
         (types abbrev utils union substitute)
         (rep type-rep)
         (env tvar-env)

         ;; fixme - don't need to be bound in this phase - only to make tests work
         (only-in '#%kernel [reverse k:reverse])
         ;; end fixme

         (for-template
          racket/base
          (only-in '#%kernel [reverse k:reverse])))


(import tc-expr^)
(export tc-app-list^)


(define (tc/app-list form expected)
  (syntax-parse form
    #:literals (#%plain-app
                reverse k:reverse list list*
                cons map andmap ormap)
    [(#%plain-app op:special-op args ...) #f]
    [(#%plain-app map f arg0 arg ...)
     (match* ((single-value #'arg0) (map single-value (syntax->list #'(arg ...))))
       ;; if the argument is a ListDots
       [((tc-result1: (ListDots: t0 bound0))
         (list (tc-result1: (or (and (ListDots: t bound) (app (λ _ #f) var))
                                ;; a devious hack - just generate #f so the test below succeeds
                                ;; have to explicitly bind `var' since otherwise `var' appears
                                ;; on only one side of the or
                                ;; NOTE: safe to include these, `map' will error if any list is
                                ;; not the same length as all the others
                                (and (Listof: t var) (app (λ _ #f) bound))))
               ...))
        (=> fail)
        (unless (for/and ([b bound]) (or (not b) (eq? bound0 b))) (fail))
        (match (extend-tvars (list bound0)
                 ;; just check that the function applies successfully to the element type
                 (tc/funapp #'f #'(arg0 arg ...) (tc-expr #'f) (cons (ret t0) (map ret t)) expected))
          [(tc-result1: t) (ret (make-ListDots t bound0))]
          [(tc-results: ts)
           (tc-error/expr #:return (ret (Un))
                          "Expected one value, but got ~a" (-values ts))])]
       ;; otherwise, if it's not a ListDots, defer to the regular function typechecking
       [(res0 res) #f])]
    ;; ormap/andmap of ... argument
    [(#%plain-app (~or andmap ormap) f arg)
     ;; check the arguments
     (match-let* ([arg-ty (single-value #'arg)]
                  [ft (tc-expr #'f)])
       (match (match arg-ty
                ;; if the argument is a ListDots
                [(tc-result1: (ListDots: t bound))
                 ;; just check that the function applies successfully to the element type
                 (tc/funapp #'f #'(arg) ft (list (ret (substitute Univ bound t))) expected)]
                ;; otherwise ...
                [_ #f])
         [(tc-result1: t) (ret (Un (-val #f) t))]
         ;; if it's not a ListDots, defer to the regular function typechecking
         [_ #f]))]
    ;; special case for `list'
    [(#%plain-app list . args)
     (match expected
       [(tc-result1: (Listof: elem-ty))
        (for ([i (in-list (syntax->list #'args))])
             (tc-expr/check i (ret elem-ty)))
        expected]
       [(tc-result1: (List: (? (lambda (ts) (= (length (syntax->list #'args))
                                               (length ts)))
                               ts)))
        (for ([ac (in-list (syntax->list #'args))]
              [exp (in-list ts)])
             (tc-expr/check ac (ret exp)))
        expected]
       [_
        (let ([tys (map tc-expr/t (syntax->list #'args))])
          (ret (apply -lst* tys)))])]
    ;; special case for `list*'
    [(#%plain-app list* . args)
     (match-let* ([(list tys ... last) (map tc-expr/t (syntax->list #'args))])
       (ret (foldr -pair last tys)))]
    ;; special case for `reverse' to propagate expected type info
    [(#%plain-app (~or reverse k:reverse) arg)
     (match expected
       [(tc-result1: (Listof: _))
        (tc-expr/check #'arg expected)]
       [(tc-result1: (List: ts))
        (tc-expr/check #'arg (ret (-Tuple (reverse ts))))
        expected]
       [_
        (match (single-value #'arg)
          [(tc-result1: (List: ts))
           (cond-check-below (ret (-Tuple (reverse ts))) expected)]
          [arg-ty
           (tc/funapp #'reverse #'(arg) (single-value #'reverse) (list arg-ty) expected)])])]
    [_ #f]))



