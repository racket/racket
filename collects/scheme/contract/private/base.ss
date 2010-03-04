#lang scheme/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide contract
         recursive-contract
         current-contract-region
         has-contract?
         get-contract)

(require (for-syntax scheme/base)
         scheme/stxparam
         unstable/srcloc
         unstable/location
         "guts.ss"
         "blame.ss")

(define-syntax-parameter current-contract-region
  (λ (stx) #'(quote-module-path)))

(define-syntax (contract stx)
  (syntax-case stx ()
    [(_ c v pos neg name loc)
     (syntax/loc stx
       (apply-contract c v pos neg name loc))]
    [(_ c v pos neg)
     (syntax/loc stx
       (apply-contract c v pos neg #f (build-source-location #f)))]
    [(_ c v pos neg src)
     (raise-syntax-error 'contract
       (string-append
        "please update contract application to new protocol "
        "(either 4 or 6 arguments)"))]))

(define (apply-contract c v pos neg name loc)
  (let* ([c (coerce-contract 'contract c)])
    (check-source-location! 'contract loc)
    (remember-contract
     (((contract-projection c)
       (make-blame loc name (contract-name c) pos neg #t))
      v)
     c)))

(define-struct contracted-function (f contract) #:property prop:procedure 0)
(define (remember-contract f contract)
  (cond
    [(parameter? f) f]
    [(procedure? f) (make-contracted-function f contract)]
    [else f]))

(define (has-contract? x) (contracted-function? x))
(define (get-contract x) 
  (unless (has-contract? x)
    (raise-type-error 'get-contract
                      "<has-contract>"
                      x))
  (contracted-function-contract x))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax
      (make-contract
       #:name '(recursive-contract arg)
       #:projection
       (λ (blame)
          (let ([ctc (coerce-contract 'recursive-contract arg)])
            (let ([f (contract-projection ctc)])
              (λ (val)
                 ((f blame) val)))))))]))
