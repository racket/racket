#lang racket/base
(require "intf.rkt"
         "local-member-names.rkt"
         "annotate.rkt"
         "colors.rkt"
         syntax/boundmap
         syntax/kerncase
         string-constants)
(provide annotate-contracts)

(define (annotate-contracts stx low-binders binding-inits)
  (define boundary-start-map (make-hash))
  (define internal-start-map (make-hash))
  (define domain-map (make-hash))
  (define range-map (make-hash))
  
  ;; coloring-plans : hash[stx -o-> (listof color)]
  (define coloring-plans (make-hash))
  
  (let loop ([stx stx])
    (add-to-map stx 'racket/contract:contract-on-boundary boundary-start-map)
    (add-to-map stx 'racket/contract:internal-contract internal-start-map)
    (add-to-map stx 'racket/contract:negative-position domain-map)
    (add-to-map stx 'racket/contract:positive-position range-map)
    (syntax-case stx ()
      [(a . b) (loop #'a) (loop #'b)]
      [_ (void)]))
  
  ;; fill in the coloring-plans table for boundary contracts
  (for ([(start-k start-val) (in-hash boundary-start-map)])
    (for ([start-stx (in-list start-val)])
      (do-contract-traversal start-stx #t
                             coloring-plans
                             low-binders binding-inits
                             domain-map range-map
                             #t)))
  
  ;; fill in the coloring-plans table for internal contracts
  (for ([(start-k start-val) (in-hash internal-start-map)])
    (for ([start-stx (in-list start-val)])
      (do-contract-traversal start-stx #f
                             coloring-plans
                             low-binders binding-inits
                             domain-map range-map
                             #f)))
  
  ;; enact the coloring plans
  (for ([(stx colors) (in-hash coloring-plans)])
    (cond
      [(and (member my-obligation-style-name colors)
            (member their-obligation-style-name colors))
       (add-mouse-over stx (string-constant cs-contract-both-obligation))]
      [(member my-obligation-style-name colors)
       (add-mouse-over stx (string-constant cs-contract-my-obligation))]
      [(member their-obligation-style-name colors)
       (add-mouse-over stx (string-constant cs-contract-their-obligation))]
      [(member unk-obligation-style-name colors)
       (add-mouse-over stx (string-constant cs-contract-unk-obligation))])))

(define (do-contract-traversal start-stx boundary-contract?
                               coloring-plans
                               low-binders binding-inits domain-map range-map polarity)
  (let ploop ([stx start-stx]
              [polarity polarity])
    
    (define (call-give-up)
      (give-up start-stx boundary-contract? coloring-plans))
    
    (let ([main-prop (syntax-property stx 'racket/contract:contract)])
      (cond
        [main-prop
         ;; we've found a contract, now go color it and 
         ;; continue with the sub-contract expressions (as indicated via the properties)
         (let sloop ([prop main-prop])
           (cond
             [(pair? prop) (sloop (car prop)) (sloop (cdr prop))]
             [(vector? prop)
              (let ([id (vector-ref prop 0)]
                    [to-color-pos (vector-ref prop 1)]
                    [to-color-neg (vector-ref prop 2)])
                (for ((stx (in-list to-color-pos)))
                  (base-color stx polarity boundary-contract? coloring-plans))
                (for ((stx (in-list to-color-neg)))
                  (base-color stx (not polarity) boundary-contract? coloring-plans))
                (for ((stx (in-list (hash-ref domain-map id '()))))
                  (do-contract-traversal stx boundary-contract?
                                         coloring-plans
                                         low-binders binding-inits domain-map range-map (not polarity)))
                (for ((stx (in-list (hash-ref range-map id '()))))
                  (do-contract-traversal stx boundary-contract?
                                         coloring-plans
                                         low-binders binding-inits domain-map range-map polarity)))]))]
        
        [else
         ;; we didn't find a contract, but we might find one in a subexpression
         ;; so we need to go look for it (possibly giving up)
         (kernel-syntax-case stx #f
           [(#%expression expr)
            (ploop #'expr polarity)]
           [(module id name-id (#%plain-module-begin mod-level-form ...))
            (call-give-up)]
           [(begin tl-form ... last-one)
            (ploop #'last-one polarity)]
           [(#%provide pvd ...)
            (call-give-up)]
           [(#%declare decl ...)
            (call-give-up)]
           [(define-values (id ...) expr)
            (call-give-up)]
           [(define-syntaxes (id ...) expr)
            (call-give-up)]
           [(begin-for-syntax (id ...) expr)
            (call-give-up)]
           [(#%require rspec ...)
            (call-give-up)]
           [id
            (identifier? #'id)
            (if (known-predicate? #'id)
                (base-color #'id polarity boundary-contract? coloring-plans)
                (let ([binders (module-identifier-mapping-get low-binders #'id (λ () #f))])
                  (if binders
                      (begin
                        (base-color #'id polarity boundary-contract? coloring-plans)
                        (for ((binder (in-list binders)))
                          (base-color binder polarity boundary-contract? coloring-plans)
                          (for ((rhs (in-list (module-identifier-mapping-get binding-inits binder (λ () '())))))
                            (ploop rhs polarity))))
                      (call-give-up))))]
           [const
            (let ([val (syntax-e #'const)])
              (or (boolean? val)
                  (number? val)
                  (string? val)
                  (char? val)
                  (regexp? val)))
            (base-color stx polarity boundary-contract? coloring-plans)]
           [(#%plain-lambda (id) expr ...)
            (identifier? #'id)
            (base-color stx polarity boundary-contract? coloring-plans)]
           [(#%plain-lambda id expr ...)
            (identifier? #'id)
            (base-color stx polarity boundary-contract? coloring-plans)]
           [(#%plain-lambda formals expr ...)
            (call-give-up)]
           [(case-lambda [formals expr] ...)
            ;; this should really only happen when the arity of the case-lambda includes 1
            ;; (otherwise we should call give-up)
            (base-color stx polarity boundary-contract? coloring-plans)]
           [(if a b c)
            ;; these calls are questionable. 
            ;; if we ultimately end up giving up in both
            ;; branches, maybe we should actually be coloring the entire thing
            ;; in the blank color, but we'll only color the then and else branches
            ;; in that color with this code.
            ;; on the other hand, recurring like this will mean that the two
            ;; branches are considered separately and thus calling give-up
            ;; on one side will not pollute the other side.
            (do-contract-traversal #'b boundary-contract?
                                   coloring-plans
                                   low-binders binding-inits domain-map range-map polarity)
            (do-contract-traversal #'c boundary-contract?
                                   coloring-plans
                                   low-binders binding-inits domain-map range-map polarity)]
           ;; [(begin expr ...) (void)]
           [(begin0 fst rst ...)
            (ploop #'fst polarity)]
           [(let-values bindings body ... last-one)
            (ploop #'last-one polarity)]
           [(letrec-values bindings body ... last-one)
            (ploop #'last-one polarity)]
           [(set! a b)
            (call-give-up)]
           [(quote stuff)
            (base-color stx polarity boundary-contract? coloring-plans)]
           [(quote-syntax stuff)
            (call-give-up)]
           [(with-continuation-mark a b c)
            (ploop #'c polarity)]
           [(#%plain-app f args ...)
            (call-give-up)]
           [(#%top . id)
            (call-give-up)]
           [(#%variable-reference ignored ...)
            (call-give-up)]
           [_ 
            (begin
              #;(error 'contract-traversal.rkt "unknown thing: ~s\n" stx)
              (call-give-up))
              ])]))))

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

;; returns #t if the result is known to be a predicate that should correspond to a
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

(define (give-up stx boundary-contract? coloring-plans)
  (let loop ([stx stx])
    (when (syntax-original? stx)
      (blank-color stx boundary-contract? coloring-plans))
    
    (let oloop ([origin (syntax-property stx 'origin)])
      (cond
        [(pair? origin) (oloop (car origin)) (oloop (cdr origin))]
        [(syntax? origin) 
         (when (syntax-original? origin)
           (blank-color origin boundary-contract? coloring-plans))]))
    
    (syntax-case stx ()
      [(a . b) (loop #'a) (loop #'b)]
      [_ (void)])))

(define (base-color stx polarity boundary-contract? coloring-plans)
  (make-a-coloring-plan
   stx
   (if polarity my-obligation-style-name their-obligation-style-name)
   coloring-plans))

(define (blank-color stx boundary-contract? coloring-plans)
  (make-a-coloring-plan stx unk-obligation-style-name coloring-plans))

(define (make-a-coloring-plan stx plan coloring-plans)
  (hash-set! coloring-plans
             stx
             (cons 
              plan
              (hash-ref coloring-plans stx '()))))
