#lang racket/base
(require "intf.rkt" 
         "annotate.rkt"
         "colors.rkt"
         syntax/kerncase)
(provide annotate-contracts)

(define (annotate-contracts stx)
  (define start-map (make-hash))
  (define arrow-map (make-hash))
  (define domain-map (make-hash))
  (define range-map (make-hash))
  
  (let loop ([stx stx])
    (add-to-map stx 'racket/contract:contract-on-boundary start-map)
    (add-to-map stx 'racket/contract:domain-of domain-map)
    (add-to-map stx 'racket/contract:rng-of range-map)
    (add-to-map stx 'racket/contract:function-contract arrow-map)
    (syntax-case stx ()
      [(a . b) (loop #'a) (loop #'b)]
      [else (void)]))
  
  (for ([(start-k start-val) (in-hash start-map)])
    (for ([start-stx (in-list start-val)])
      (do-contract-traversal start-stx arrow-map domain-map range-map #t))))

(define (do-contract-traversal start-stx arrow-map domain-map range-map polarity)
  (let loop ([stx start-stx])
    (base-color stx polarity)
    (kernel-syntax-case stx #f
      [(#%expression expr)
       (loop #'expr)]
      [(module id name-id (#%plain-module-begin mod-level-form ...))
       (for-each loop (syntax->list #'(mod-level-form ...)))]
      [(begin tl-form ... last-one)
       (loop #'last-one)]
      [(#%provide pvd ...)
       (void)]
      [(define-values (id ...) expr)
       (void)]
      [(define-syntaxes (id ...) expr)
       (void)]
      [(define-values-for-syntax (id ...) expr)
       (void)]
      [(#%require rspec ...)
       (void)]
      [id
       (identifier? #'id)
       (void)]
      [(#%plain-lambda formals expr ...)
       (void)]
      [(case-lambda [formals expr] ...)
       (void)]
      [(if a b c)
       (loop #'b)
       (loop #'c)]
      ;; [(begin expr ...) (void)]
      [(begin0 fst rst ...)
       (loop #'fst)]
      [(let-values bindings body ... last-one)
       (loop #'last-one)]
      [(letrec-values bindings body ... last-one)
       (loop #'last-one)]
      [(set! a b)
       (void)]
      [(quote stuff)
       (void)]
      [(quote-syntax stuff)
       (void)]
      [(with-continuation-mark a b c)
       (loop #'c)]
      [(#%plain-app f args ...)
       (void)]
      [(#%top . id)
       (void)]
      [(#%variable-reference id)
       (void)]
      [(#%variable-reference)
       (void)])))
       
       
       
;; add-to-map : syntax any hash[any -> (listof stx)]
;; looks at stx's property prop and updates map,
;; using the value of the property as the key
(define (add-to-map stx prop map)
  (let loop ([val (syntax-property stx prop)])
    (cond
      [(symbol? val)
       (hash-set! map val (cons stx (hash-ref map val '())))]
      [(pair? val)
       (loop (car val))
       (loop (cdr val))])))

#|
    (define (annotate-contracts stx)
      (let loop ([stx stx])
        (let sloop ([prop (syntax-property stx 'provide/contract-original-contract)])
          (cond
            [(vector? prop)
             (color-obligations (vector-ref prop 1))]
            [(pair? prop) (sloop (car prop)) 
                          (sloop (cdr prop))]))
        (syntax-case stx ()
          [(a . b) (loop #'a) (loop #'b)]
          [else (void)])))
    
    (define (color-obligations stx)
      (let loop ([stx stx]
                 [polarity #t])
      (syntax-case stx (->)
        [(-> a ... rng)
         (begin
           (base-color (car (syntax-e stx)) polarity)
           (for-each (λ (x) (loop x (not polarity))) (syntax->list #'(a ...)))
           (syntax-case #'rng (values any)
             [(values b ...)
              (for-each (λ (x) (loop x polarity)) (syntax->list #'(b ...)))]
             [any
              (void)]
             [rng
              (loop #'rng polarity)]))]
        [id
         (and (identifier? #'id)
              (known-predicate? #'id))
         (base-color stx polarity)]
        [else
         (color stx unk-obligation-style-name 'contract-mode)])))
|#
;; returns #t if the result is known to be a predicate that shoudl correspond to a
;; complete obligation for the contract. If it is some unknown variable, this variable
;; may refer to some other contract with nested obligations, so we have to return #f here.
;; approximate this by just asking 'did this identifier come from the core?' (which is known
;; to not bind any contracts (I hope))
(define (known-predicate? id)
  (let ([ib (identifier-binding id)])
    (and (list? ib)
         (let ([src (list-ref ib 0)])
           (let-values ([(base rel) (module-path-index-split src)])
             (member base '('#%kernel racket racket/base scheme scheme/base)))))))

(define (base-color stx polarity)
  (printf "base-color ~s\n" stx)
  (color stx 
         (if polarity my-obligation-style-name their-obligation-style-name) 
         'contract-mode))
