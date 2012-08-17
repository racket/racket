#lang racket/unit

(require "../utils/utils.rkt"
         "tc-app/signatures.rkt"
         syntax/parse racket/match 
         (typecheck signatures check-below tc-funapp)
         (types utils abbrev)
         (rep type-rep filter-rep object-rep rep-utils))

(import tc-expr^ tc-app-keywords^
        tc-app-hetero^ tc-app-list^ tc-app-apply^ tc-app-values^
        tc-app-objects^ tc-app-eq^ tc-app-lambda^ tc-app-special^)
(export tc-app^)


;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (or
    (tc/app-annotated form expected)
    (tc/app-hetero form expected)
    (tc/app-list form expected)
    (tc/app-apply form expected)
    (tc/app-values form expected)
    (tc/app-keywords form expected)
    (tc/app-objects form expected)
    (tc/app-eq form expected)
    (tc/app-lambda form expected)
    (tc/app-special form expected)
    (tc/app-regular form expected)))


(define-syntax-class annotated-op
  (pattern i:identifier
           #:when (or (syntax-property #'i 'type-inst)
                      (syntax-property #'i 'type-ascription))))

(define (tc/app-annotated form expected)
  (syntax-parse form
    #:literals (#%plain-app)
    ;; Just do regular typechecking if we have one of these.
    [(#%plain-app rator:annotated-op . rands) (tc/app-regular form expected)]
    [_ #f]))

(define (tc/app-regular form expected)
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
