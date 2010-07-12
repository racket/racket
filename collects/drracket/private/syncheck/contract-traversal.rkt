#lang racket/base
(require "intf.rkt" 
         "annotate.rkt"
         "colors.rkt"
         syntax/boundmap
         syntax/kerncase)
(provide annotate-contracts)

(define (annotate-contracts stx low-binders varrefs)
  (define start-map (make-hash))
  (define arrow-map (make-hash))
  (define domain-map (make-hash))
  (define range-map (make-hash))
  
  ;; coloring-plans : hash[stx -o-> (listof color)]
  (define coloring-plans (make-hash))
  
  (let loop ([stx stx])
    (add-to-map stx 'racket/contract:contract-on-boundary start-map)
    (add-to-map stx 'racket/contract:domain-of domain-map)
    (add-to-map stx 'racket/contract:rng-of range-map)
    (add-to-map stx 'racket/contract:function-contract arrow-map)
    (syntax-case stx ()
      [(a . b) (loop #'a) (loop #'b)]
      [else (void)]))
  
  ;; fill in the coloring-plans table
  (for ([(start-k start-val) (in-hash start-map)])
    (for ([start-stx (in-list start-val)])
      (do-contract-traversal start-stx
                             coloring-plans low-binders 
                             arrow-map domain-map range-map
                             #t)))
  
  ;; enact the coloring plans
  (for ([(stx colors) (in-hash coloring-plans)])
    (cond
      [(and (member my-obligation-style-name colors)
            (member their-obligation-style-name colors))
       (color stx both-obligation-style-name 'contract-mode)]
      [(member my-obligation-style-name colors)
       (color stx my-obligation-style-name 'contract-mode)]
      [(member their-obligation-style-name colors)
       (color stx their-obligation-style-name 'contract-mode)]
      [(member unk-obligation-style-name colors)
       (color stx unk-obligation-style-name 'contract-mode)])))

(define (do-contract-traversal start-stx coloring-plans low-binders arrow-map domain-map range-map polarity)
  (let ploop ([stx start-stx]
              [polarity polarity])
    
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
                    [to-color (vector-ref prop 1)])
                (base-color to-color polarity coloring-plans)
                (for ((stx (in-list (hash-ref domain-map id '()))))
                  (do-contract-traversal stx coloring-plans low-binders arrow-map domain-map range-map (not polarity)))
                (for ((stx (in-list (hash-ref range-map id '()))))
                  (do-contract-traversal stx coloring-plans low-binders arrow-map domain-map range-map polarity)))]))]
        
        [else
         ;; we didn't find a contract, but we might find one in a subexpression
         ;; so we need to go look for it (possibly giving up)
         (kernel-syntax-case stx #f
           [(#%expression expr)
            (ploop #'expr polarity)]
           [(module id name-id (#%plain-module-begin mod-level-form ...))
            (give-up start-stx coloring-plans)]
           [(begin tl-form ... last-one)
            (ploop #'last-one polarity)]
           [(#%provide pvd ...)
            (give-up start-stx coloring-plans)]
           [(define-values (id ...) expr)
            (give-up start-stx coloring-plans)]
           [(define-syntaxes (id ...) expr)
            (give-up start-stx coloring-plans)]
           [(define-values-for-syntax (id ...) expr)
            (give-up start-stx coloring-plans)]
           [(#%require rspec ...)
            (give-up start-stx coloring-plans)]
           [id
            (identifier? #'id)
            (if (known-predicate? #'id)
                (base-color #'id polarity coloring-plans)
                (begin
                  ;(printf "mapped to ~s\n" (module-identifier-mapping-get low-binders #'id))
                  (give-up start-stx coloring-plans)))]
           [(#%plain-lambda formals expr ...)
            (give-up start-stx coloring-plans)]
           [(case-lambda [formals expr] ...)
            (give-up start-stx coloring-plans)]
           [(if a b c)
            ;; these calls are questionable. 
            ;; if we ultimately end up giving up in both
            ;; branches, maybe we should actually be coloring the entire thing
            ;; in the blank color, but we'll only color the then and else branches
            ;; in that color with this code.
            ;; on the other hand, recurring like this will mean that the two
            ;; branches are considered separately and thus calling give-up
            ;; on one side will not pollute the other side.
            (do-contract-traversal #'b coloring-plans low-binders arrow-map domain-map range-map polarity)
            (do-contract-traversal #'c coloring-plans low-binders arrow-map domain-map range-map polarity)]
           ;; [(begin expr ...) (void)]
           [(begin0 fst rst ...)
            (ploop #'fst polarity)]
           [(let-values bindings body ... last-one)
            (ploop #'last-one polarity)]
           [(letrec-values bindings body ... last-one)
            (ploop #'last-one polarity)]
           [(set! a b)
            (give-up start-stx coloring-plans)]
           [(quote stuff)
            (give-up start-stx coloring-plans)]
           [(quote-syntax stuff)
            (give-up start-stx coloring-plans)]
           [(with-continuation-mark a b c)
            (ploop #'c polarity)]
           [(#%plain-app f args ...)
            (give-up start-stx coloring-plans)]
           [(#%top . id)
            (give-up start-stx coloring-plans)]
           [(#%variable-reference ignored ...)
            (give-up start-stx coloring-plans)])]))))

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

(define (give-up stx coloring-plans) 
  (let loop ([stx stx])
    (when (syntax-original? stx)
      (blank-color stx coloring-plans))
    
    (let oloop ([origin (syntax-property stx 'origin)])
      (cond
        [(pair? origin) (oloop (car origin)) (oloop (cdr origin))]
        [(syntax? origin) 
         (when (syntax-original? origin)
           (blank-color origin coloring-plans))]))
    
    (syntax-case stx ()
      [(a . b) (loop #'a) (loop #'b)]
      [_ (void)])))



(define (base-color stx polarity coloring-plans)
  (make-a-coloring-plan stx
                        (if polarity my-obligation-style-name their-obligation-style-name)
                        coloring-plans))

(define (blank-color stx coloring-plans)
  (make-a-coloring-plan stx unk-obligation-style-name coloring-plans))

(define (make-a-coloring-plan stx plan coloring-plans)
  (hash-set! coloring-plans
             stx
             (cons 
              plan
              (hash-ref coloring-plans stx '()))))
