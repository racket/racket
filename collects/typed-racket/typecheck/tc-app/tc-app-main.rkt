#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match 
         syntax/parse/experimental/reflect
         (typecheck signatures check-below tc-funapp tc-app-helper)
         (types utils abbrev)
         (rep type-rep filter-rep object-rep rep-utils)
         (for-template racket/base))

(import tc-expr^ tc-app-keywords^
        tc-app-hetero^ tc-app-list^ tc-app-apply^ tc-app-values^
        tc-app-objects^ tc-app-eq^ tc-app-lambda^ tc-app-special^)
(export tc-app^)


(define-syntax-class annotated-op
  (pattern i:identifier
           #:when (or (syntax-property #'i 'type-inst)
                      (syntax-property #'i 'type-ascription))))

(define-tc/app-syntax-class (tc/app-annotated expected)
  ;; Just do regular typechecking if we have one of these.
  (pattern (~and form (rator:annotated-op . rands))
    (tc/app-regular #'form expected)))

(define-tc/app-syntax-class (tc/app-regular* expected)
  (pattern form (tc/app-regular #'form expected)))

(define-syntax-rule (combine-tc/app-syntax-classes class-name case ...)
  (define-syntax-class (class-name expected)
    #:attributes (check)
    (pattern (~reflect v (case expected) #:attributes (check))
             #:attr check (attribute v.check)) ...))

(combine-tc/app-syntax-classes tc/app-special-cases
  tc/app-annotated
  tc/app-list
  tc/app-apply
  tc/app-eq
  tc/app-hetero
  tc/app-values
  tc/app-keywords
  tc/app-objects
  tc/app-lambda
  tc/app-special
  tc/app-regular*)

;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    [(#%plain-app . (~var v (tc/app-special-cases expected)))
     ((attribute v.check))]))



(define (tc/app-regular form expected)
  (syntax-case form ()
    [(f . args)
     (let* ([f-ty (single-value #'f)]
            [args* (syntax->list #'args)])
       (match f-ty
         [(tc-result1:
           (and t (Function:
                   (list (and a (arr: (? (λ (d) (= (length d) (length args*))) dom)
                                      (Values: (list (Result: v (FilterSet: (Top:) (Top:)) (Empty:))))
                                      #f #f (list (Keyword: _ _ #f) ...)))))))
          (for ([a (in-list args*)] [t (in-list dom)])
            (tc-expr/check a (ret t)))
          (ret v)]
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
