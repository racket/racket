#lang racket/base
(require "private/prop.rkt"
         (prefix-in : (only-in "private/prop.rkt"
                               build-chaperone-contract-property
                               build-flat-contract-property
                               make-chaperone-contract
                               make-flat-contract))
         "private/guts.rkt"
         "private/blame.rkt")

(provide 
 prop:contract
 contract-struct-late-neg-projection
 contract-struct-generate
 contract-struct-exercise
 contract-struct-list-contract?
 
 prop:flat-contract
 prop:chaperone-contract
 
 contract-property?
 build-contract-property
 
 chaperone-contract-property?
 
 flat-contract-property?
 
 make-contract
 
 prop:opt-chaperone-contract
 prop:opt-chaperone-contract?
 prop:opt-chaperone-contract-get-test
 
 prop:orc-contract
 prop:orc-contract?
 prop:orc-contract-get-subcontracts
 
 prop:recursive-contract
 prop:recursive-contract?
 prop:recursive-contract-unroll
 
 prop:arrow-contract 
 prop:arrow-contract?
 prop:arrow-contract-get-info
 (struct-out arrow-contract-info)
 
 coerce-contract
 coerce-contracts
 coerce-flat-contract
 coerce-flat-contracts
 coerce-chaperone-contract
 coerce-chaperone-contracts
 coerce-contract/f
 
 build-compound-type-name
 
 contract-stronger?
 list-contract?
 
 contract-first-order
 contract-first-order-passes?
 
 prop:contracted prop:blame
 impersonator-prop:contracted impersonator-prop:blame
 has-blame? value-blame
 
 ;; helpers for adding properties that check syntax uses
 define/final-prop
 define/subexpression-pos-prop
 define/subexpression-pos-prop/name
 
 eq-contract?
 eq-contract-val
 equal-contract?
 equal-contract-val
 char-in/c
 
 contract-continuation-mark-key
 with-contract-continuation-mark
 
 (struct-out wrapped-extra-arg-arrow)
 contract-custom-write-property-proc
 (rename-out [contract-custom-write-property-proc custom-write-property-proc])
 
 set-some-basic-contracts!
 
 blame?
 blame-source
 blame-positive
 blame-negative
 blame-contract
 blame-value
 blame-original?
 blame-swapped?
 blame-swap
 blame-replace-negative ;; used for indy blame
 blame-update ;; used for option contract transfers
 blame-add-context
 blame-add-unknown-context
 blame-context
 
 blame-add-missing-party
 blame-missing-party?
 
 raise-blame-error
 current-blame-format
 (struct-out exn:fail:contract:blame)
 blame-fmt->-string
 
 (rename-out [-make-chaperone-contract make-chaperone-contract]
             [-make-flat-contract make-flat-contract]
             [-build-chaperone-contract-property build-chaperone-contract-property]
             [-build-flat-contract-property build-flat-contract-property])
 skip-projection-wrapper?)

(define skip-projection-wrapper? (make-parameter #f))

(define (maybe-add-wrapper f x)
  (cond
    [(and x (not (skip-projection-wrapper?)))
     (f x)]
    [else x]))

(define -make-chaperone-contract
  (let ([make-chaperone-contract
         (λ (#:name [name 'anonymous-chaperone-contract]
                    #:first-order [first-order (λ (x) #t)]
                    #:late-neg-projection [late-neg-projection #f]
                    #:val-first-projection [val-first-projection #f]
                    #:projection [projection #f]
                    #:stronger [stronger #f]
                    #:list-contract? [is-list-contract #f])
           (:make-chaperone-contract
            #:name name
            #:first-order first-order
            #:late-neg-projection
            (maybe-add-wrapper add-late-neg-chaperone-check late-neg-projection)
            #:val-first-projection
            (maybe-add-wrapper add-val-first-chaperone-check val-first-projection)
            #:projection
            (maybe-add-wrapper add-projection-chaperone-check projection)
            #:stronger stronger
            #:list-contract? is-list-contract))])
    make-chaperone-contract))

(define -build-chaperone-contract-property
  (let ()
    (define (build-chaperone-contract-property
             #:name [get-name (λ (c) 'anonymous-chaperone-contract)]
             #:first-order [get-first-order (λ (c) (λ (x) #t))]
             #:val-first-projection [val-first-proj #f]
             #:late-neg-projection [late-neg-proj #f]
             #:projection [get-projection #f]
             #:stronger [stronger #f]
             #:generate [generate #f]
             #:exercise [exercise #f])
      (:build-chaperone-contract-property
       #:name get-name
       #:first-order get-first-order
       #:val-first-projection
       (maybe-add-wrapper add-prop-val-first-chaperone-check val-first-proj)
       #:late-neg-projection
       (maybe-add-wrapper add-prop-late-neg-chaperone-check late-neg-proj)
       #:projection
       (maybe-add-wrapper add-prop-chaperone-check get-projection)
       #:stronger stronger
       #:generate generate
       #:exercise exercise))
    build-chaperone-contract-property))

(define (add-prop-late-neg-chaperone-check get-late-neg)
  (λ (c)
    (add-late-neg-chaperone-check (get-late-neg c))))

(define (add-late-neg-chaperone-check accepts-blame)
  (λ (b)
    (define accepts-val-and-np (accepts-blame b))
    (λ (x neg-party)
      (check-and-signal x
                        (accepts-val-and-np x neg-party)
                        'make-chaperone-contract::late-neg-projection))))

(define (add-prop-val-first-chaperone-check get)
  (λ (c)
    (add-val-first-chaperone-check (get c))))

(define (add-val-first-chaperone-check vfp)
  (λ (b)
    (define x-acceptor (vfp b))
    (λ (x)
      (define neg-acceptor (x-acceptor x))
      (λ (neg-party)
        (check-and-signal x
                          (neg-acceptor neg-party)
                          'make-chaperone-contract::late-neg-projection)))))

(define (add-prop-chaperone-check get)
  (λ (c)
    (add-projection-chaperone-check (get c))))

(define (add-projection-chaperone-check proj)
  (λ (b)
    (define x-acceptor (proj b))
    (λ (x)
      (check-and-signal x (x-acceptor x)
                        'make-chaperone-contract::projection))))
           

(define (check-and-signal val chapd-val who)
  (unless (chaperone-of? chapd-val val)
    (raise-result-error who
                        (format "chaperone-of ~e" val)
                        chapd-val))
  chapd-val)

(define -make-flat-contract
  (let ([make-flat-contract
         (λ (#:name [name 'anonymous-chaperone-contract]
                    #:first-order [first-order (λ (x) #t)]
                    #:late-neg-projection [late-neg-projection #f]
                    #:val-first-projection [val-first-projection #f]
                    #:projection [projection #f]
                    #:stronger [stronger #f]
                    #:list-contract? [is-list-contract #f])
           (:make-flat-contract
            #:name name
            #:first-order first-order
            #:late-neg-projection (force-late-neg-eq late-neg-projection)
            #:val-first-projection (force-val-first-eq val-first-projection)
            #:projection (force-projection-eq projection)
            #:stronger stronger
            #:list-contract? is-list-contract))])
    make-flat-contract))

(define -build-flat-contract-property
  (let ([build-flat-contract-property
         (λ (#:name [name (λ (c) 'anonymous-chaperone-contract)]
                    #:first-order [first-order (λ (c) (λ (x) #t))]
                    #:late-neg-projection [late-neg-projection #f]
                    #:val-first-projection [val-first-projection #f]
                    #:projection [projection #f]
                    #:stronger [stronger #f]
                    #:list-contract? [is-list-contract #f])
           (:build-flat-contract-property
            #:name name
            #:first-order first-order
            #:late-neg-projection
            (and late-neg-projection (λ (c) (force-late-neg-eq (late-neg-projection c))))
            #:val-first-projection
            (and val-first-projection (λ (c) (force-val-first-eq (val-first-projection c))))
            #:projection
            (and projection (λ (c) (force-projection-eq (projection c))))
            #:stronger stronger
            #:list-contract? is-list-contract))])
    build-flat-contract-property))

(define (force-late-neg-eq accepts-blame)
  (and accepts-blame
       (λ (b)
         (define accepts-val-and-np (accepts-blame b))
         (λ (x neg-party)
           (accepts-val-and-np x neg-party)
           x))))

(define (force-val-first-eq vfp)
  (and vfp
       (λ (b)
         (define x-acceptor (vfp b))
         (λ (x)
           (define neg-acceptor (x-acceptor x))
           (λ (neg-party)
             (neg-acceptor neg-party)
             x)))))

(define (force-projection-eq proj)
  (and proj
       (λ (b)
         (define x-acceptor (proj b))
         (λ (x)
           (x-acceptor x)
           x))))
