#lang racket/base

(require (for-syntax racket/base)
         (for-template racket)
         racket/syntax)

(provide (all-defined-out)
         (all-from-out (submod "." ensures)))

(define skip-ids (syntax->list #'(+ - * / < > <= >= = min max)))

(define (skip-binding? e-stx)
  (and (identifier? e-stx)
       (findf (λ (skip-id) (free-identifier=? skip-id e-stx)) skip-ids)))

(define (maybe-binding e-stx temp-id)
  (cond [(skip-binding? e-stx)  (values (list) e-stx)]
        [else  (values (list #`[#,temp-id #,e-stx]) temp-id)]))

(define (generate-bindings e-stxs)
  (let ([e-stxs  (if (list? e-stxs) e-stxs (syntax->list e-stxs))])
    (define-values (bnds refs)
      (for/lists (bnds refs) ([e-stx  (in-list e-stxs)]
                              [temp-id  (in-list (generate-temporaries e-stxs))])
        (maybe-binding e-stx temp-id)))
    (list (apply append bnds) refs)))

(module ensures racket/base
  (require racket/flonum
           "exception.rkt"
           (except-in typed/racket/base
                      raise-arguments-error
                      raise-argument-error
                      raise-result-error
                      raise-range-error))
  
  (provide (all-defined-out))

  (define-syntax-rule (ensure-index name n-expr)
    (let: ([n : Integer  n-expr])
      (if (index? n) n (raise-argument-error name "Index" n))))
  
  (define-syntax-rule (ensure-flvector name xs-expr)
    (let: ([xs : FlVector  xs-expr])
      (if (flvector? xs) xs (raise-argument-error name "FlVector" xs))))
  
  (define-syntax-rule (ensure-procedure name f-expr T)
    (let: ([f : T  f-expr])
      (if (procedure? f) f (raise-argument-error name "Procedure" f))))
  
  (define-syntax-rule (ensure-float-complex name z-expr)
    (let*: ([z : Float-Complex  z-expr]
            [x : Flonum  (if (number? z)
                             (let: ([x : Flonum  (real-part z)])
                               (if (flonum? x) x #f))
                             #f)]
            [y : Flonum  (if x
                             (let: ([y : Flonum  (imag-part z)])
                               (if (flonum? y) y #f))
                             #f)])
      (values x (if y y (raise-argument-error name "Float-Complex" z)))))
  
  )

(require 'ensures)

