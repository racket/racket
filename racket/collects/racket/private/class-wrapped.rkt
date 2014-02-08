#lang racket/base
(provide (struct-out wrapped-class-info)
         (struct-out wrapped-class)
         (struct-out wrapped-object)
         unwrap-class
         unwrap-object)

(struct wrapped-class-info (class blame 
                             neg-extra-arg-vec ;; vector that parallels the class's vector of methods
                             neg-acceptors-ht ;; range of ht has curried (neg-pary -> mth) fns
                             pos-field-projs neg-field-projs
                             init-proj-pairs)
  #:transparent)
(struct wrapped-class (the-info neg-party)
  #:property prop:custom-write
  (λ (stct port mode)
    (do-custom-write (wrapped-class-info-class (wrapped-class-the-info stct)) port mode))
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

(define (unwrap-class cls)
  (cond
    [(wrapped-class? cls) (wrapped-class-info-class (wrapped-class-the-info cls))]
    [else cls]))
