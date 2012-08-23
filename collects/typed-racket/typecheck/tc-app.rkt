#lang racket/unit

(require "../utils/utils.rkt"
         "tc-app/signatures.rkt"
         syntax/parse racket/match 
         syntax/parse/experimental/reflect
         (typecheck signatures check-below tc-funapp)
         (types utils abbrev)
         (rep type-rep filter-rep object-rep rep-utils)
         (for-template racket/base))

(import tc-expr^ tc-app-keywords^
        tc-app-hetero^ tc-app-list^ tc-app-apply^ tc-app-values^
        tc-app-objects^ tc-app-eq^ tc-app-lambda^ tc-app-special^)
(export tc-app^)


;; the main dispatching function
;; syntax tc-results? -> tc-results?
(define (tc/app/internal form expected)
  (syntax-parse form
    [(#%plain-app .
       (~or (~var v (tc/app-annotated expected))
            (~reflect v (tc/app-list expected) #:attributes (check))
            (~reflect v (tc/app-apply expected) #:attributes (check))
            (~reflect v (tc/app-eq expected) #:attributes (check))
            (~reflect v (tc/app-hetero expected) #:attributes (check))
            (~reflect v (tc/app-values expected) #:attributes (check))
            (~reflect v (tc/app-keywords expected) #:attributes (check))
            (~reflect v (tc/app-objects expected) #:attributes (check))
            (~reflect v (tc/app-lambda expected) #:attributes (check))
            (~reflect v (tc/app-special expected) #:attributes (check))
            (~var v (tc/app-regular* expected))))
     ((attribute v.check))]))

(define-syntax-class annotated-op
  (pattern i:identifier
           #:when (or (syntax-property #'i 'type-inst)
                      (syntax-property #'i 'type-ascription))))


(define-syntax-class (tc/app-annotated expected)
  ;; Just do regular typechecking if we have one of these.
  (pattern (~and form (rator:annotated-op . rands))
   #:attr check (lambda () (tc/app-regular #'form expected))))

(define-syntax-class (tc/app-regular* expected)
  (pattern form 
   #:attr check (lambda () (tc/app-regular #'form expected))))

(define (tc/app-regular form expected)
  (syntax-parse form
    [(f . args)
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
