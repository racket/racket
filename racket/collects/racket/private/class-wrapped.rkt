#lang racket/base
(provide (except-out (struct-out wrapped-class-info) wrapped-class-info?)
         (struct-out wrapped-object)
         unwrap-object
         
         impersonator-prop:wrapped-class-info 
         impersonator-prop:has-wrapped-class-info?
         impersonator-prop:get-wrapped-class-info

         impersonator-prop:wrapped-class-neg-party
         impersonator-prop:has-wrapped-class-neg-party?
         impersonator-prop:get-wrapped-class-neg-party)

(struct wrapped-class-info (blame 
                            neg-extra-arg-vec ;; vector that parallels the class's vector of methods
                            neg-acceptors-ht ;; range of ht has curried (neg-pary -> mth) fns
                            pos-field-projs neg-field-projs
                            init-proj-pairs)
  #:transparent)

(struct wrapped-object (object neg-extra-arg-vec pos-field-projs neg-field-projs neg-party)
  #:transparent
  #:property prop:custom-write
  (λ (stct port mode)
    (do-custom-write (wrapped-object-object stct) port mode)))

(define (do-custom-write v port mode)
  (cond
    [(custom-write? v)
     ((custom-write-accessor v) v port mode)]
    [(equal? mode #t)
     (write v port)]
    [(equal? mode #f)
     (display v port)]
    [else
     (print v port mode)]))

(define (unwrap-object o)
  (cond
    [(wrapped-object? o) (wrapped-object-object o)]
    [else o]))

(define-values (impersonator-prop:wrapped-class-info 
                impersonator-prop:has-wrapped-class-info?
                impersonator-prop:get-wrapped-class-info)
  (make-impersonator-property 'wrapped-class-info))

(define-values (impersonator-prop:wrapped-class-neg-party
                impersonator-prop:has-wrapped-class-neg-party?
                impersonator-prop:get-wrapped-class-neg-party)
  (make-impersonator-property 'wrapped-class-neg-party))
