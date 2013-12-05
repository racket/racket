#lang racket/base

(provide define/contract)

(require (for-syntax racket/base
                     syntax/srcloc
                     (prefix-in a: racket/contract/private/helpers))
         (only-in racket/contract/private/base contract))

;; First, we have the old define/contract implementation, which
;; is still used in mzlib/contract.

(define-for-syntax (make-define/contract-transformer contract-id id)
  (make-set!-transformer
   (λ (stx)
     (with-syntax ([neg-blame-str (source-location->string stx "<<unknown>>")]
                   [contract-id contract-id]
                   [id id])
       (syntax-case stx (set!)
         [(set! id arg)
          (raise-syntax-error 'define/contract
                              "cannot set! a define/contract variable"
                              stx
                              (syntax id))]
         [(f arg ...)
          (syntax/loc stx
            ((contract contract-id
                       id
                       (syntax->datum (quote-syntax f))
                       neg-blame-str
                       (quote f)
                       (quote-syntax f))
             arg
             ...))]
         [ident
          (identifier? (syntax ident))
          (syntax/loc stx
            (contract contract-id
                      id
                      (syntax->datum (quote-syntax ident))
                      neg-blame-str
                      (quote ident)
                      (quote-syntax ident)))])))))

;; (define/contract id contract expr)
;; defines `id' with `contract'; initially binding
;; it to the result of `expr'.  These variables may not be set!'d.
(define-syntax (define/contract define-stx)
  (syntax-case define-stx ()
    [(_ name contract-expr expr)
     (identifier? (syntax name))
     (with-syntax ([contract-id
                    (a:mangle-id "define/contract-contract-id"
                                 (syntax name))]
                   [id (a:mangle-id "define/contract-id"
                                    (syntax name))])
       (syntax/loc define-stx
         (begin
           (define contract-id contract-expr)
           (define-syntax name
             (make-define/contract-transformer (quote-syntax contract-id)
                                                   (quote-syntax id)))
           (define id (let ([name expr]) name))  ;; let for procedure naming
           )))]
    [(_ name contract-expr expr)
     (raise-syntax-error 'define/contract "expected identifier in first position"
                         define-stx
                         (syntax name))]))



