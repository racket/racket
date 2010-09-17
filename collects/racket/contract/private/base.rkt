#lang racket/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide contract
         recursive-contract
         current-contract-region)

(require (for-syntax racket/base)
         racket/stxparam
         unstable/srcloc
         unstable/location
         "guts.rkt"
         "blame.rkt"
         "arrow.rkt")

(define-syntax-parameter current-contract-region
  (λ (stx) #'(quote-module-path)))

(define-syntax (contract stx)
  (syntax-case stx ()
    [(_ c v pos neg name loc)
     (syntax/loc stx
       (apply-contract c v pos neg name loc (current-contract-region)))]
    [(_ c v pos neg)
     (syntax/loc stx
       (apply-contract c v pos neg #f (build-source-location #f) (current-contract-region)))]
    [(_ c v pos neg src)
     (raise-syntax-error 'contract
       (string-append
        "please update contract application to new protocol "
        "(either 4 or 6 arguments)"))]))

(define (apply-contract c v pos neg name loc usr)
  (let ([c (coerce-contract 'contract c)])
    (check-source-location! 'contract loc)
    (let ([new-val
           (((contract-projection c)
             (make-blame loc name (contract-name c) pos neg usr #t))
            v)])
      (if (and name 
               (not (parameter? new-val))  ;; when PR 11221 is fixed, remove this line
               (procedure? new-val)
               (not (eq? name (object-name new-val))))
          (cond
            [(contracted-function? new-val)
             ;; when PR11222 is fixed, change these things:
             ;;   - eliminate this cond case
             ;;   - remove the require of arrow.rkt above
             ;;   - change (struct-out contracted-function) 
             ;;     in arrow.rkt to make-contracted-function
             (make-contracted-function 
              (procedure-rename (contracted-function-proc new-val) name)
              (contracted-function-ctc new-val))]
            [else
             (procedure-rename new-val name)])
          new-val))))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax
      (make-contract
       #:name '(recursive-contract arg)
       #:first-order
       (λ (val)
         (let ([ctc (coerce-contract 'recursive-contract arg)])
           (contract-first-order-passes? ctc val)))
       #:projection
       (λ (blame)
          (let ([ctc (coerce-contract 'recursive-contract arg)])
            (let ([f (contract-projection ctc)])
              (λ (val)
                 ((f blame) val)))))))]))
