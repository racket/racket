;; should cache the count of new snips -- dont
;; use `count-snips'; use something associated with the
;; equal hash-table

#lang scheme/base

(require "private/stepper.ss"
         "private/traces.ss"
         "private/matcher.ss"
         "private/reduction-semantics.ss"
         "private/size-snip.ss"
         scheme/contract
         scheme/class
         scheme/gui/base)

(define pp-contract
  (or/c (-> any/c string?)
        (-> any/c output-port? number? (is-a?/c text%) any)))

(define ((reduction-sequence? red) terms)
  (let loop ([term (car terms)]
             [terms (cdr terms)])
    (or (null? terms)
        (and (member (car terms) (apply-reduction-relation red term))
             (loop (car terms)
                   (cdr terms))))))

(provide/contract
 [traces (->* (reduction-relation?
               any/c)
              (#:multiple?
               boolean?
               #:pred (or/c (any/c . -> . any)
                            (any/c term-node? . -> . any))
               #:pp pp-contract
               #:colors (listof any/c))
              any)]
 
 [term-node? (-> any/c boolean?)]
 [term-node-parents (-> term-node? (listof term-node?))]
 [term-node-children (-> term-node? (listof term-node?))]
 [term-node-labels (-> term-node? (listof (or/c false/c string?)))]
 [term-node-set-red! (-> term-node? boolean? void?)]
 [term-node-set-color! (-> term-node? 
                           (or/c string? (is-a?/c color%) false/c)
                           void?)]
 [term-node-expr (-> term-node? any)]
 
 [stepper
  (->* (reduction-relation?
        any/c)
       (pp-contract)
       void?)]
 [stepper/seed 
  (->* (reduction-relation?
        (cons/c any/c (listof any/c)))
       (pp-contract)
       void?)])


(provide reduction-steps-cutoff initial-font-size initial-char-width
         dark-pen-color light-pen-color dark-brush-color light-brush-color
         dark-text-color light-text-color
         default-pretty-printer)