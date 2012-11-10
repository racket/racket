;; should cache the count of new snips -- dont
;; use `count-snips'; use something associated with the
;; equal hash-table

#lang racket/base

(require "private/stepper.rkt"
         "private/traces.rkt"
         "private/matcher.rkt"
         "private/reduction-semantics.rkt"
         "private/size-snip.rkt"
         "private/show-derivations.rkt"
         "private/judgment-form.rkt"
         mrlib/graph
         racket/contract
         racket/class
         racket/gui/base)

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
               #:colors (listof (list/c string? string?))
               #:scheme-colors? boolean?
               #:racket-colors? boolean?
               #:layout (-> any/c any/c)
               #:edge-label-font (or/c #f (is-a?/c font%))
               #:edge-labels? boolean?
               #:filter (-> any/c (or/c #f string?) any/c)
               #:graph-pasteboard-mixin (make-mixin-contract graph-pasteboard<%>))
              any)]
 [traces/ps (->* (reduction-relation?
                  any/c
                  (or/c path-string? path?))
                 (#:multiple?
                  boolean?
                  #:pred (or/c (any/c . -> . any)
                               (any/c term-node? . -> . any))
                  #:pp pp-contract
                  #:colors (listof any/c)
                  #:scheme-colors? boolean?
                  #:racket-colors? boolean?
                  #:layout (-> any/c any/c)
                  #:edge-label-font (or/c #f (is-a?/c font%))
                  #:edge-labels? boolean?
                  #:filter (-> any/c (or/c #f string?) any/c)
                  #:graph-pasteboard-mixin (make-mixin-contract graph-pasteboard<%>)
                  #:post-process (-> (is-a?/c graph-pasteboard<%>) any/c))
                 any)]
 
 [show-derivations (->* ((cons/c derivation? (listof derivation?)))
                        (#:pp pp-contract
                         #:racket-colors? boolean?
                         #:init-derivation exact-nonnegative-integer?)
                        any)]
 [derivation/ps (->* (derivation? path-string?)
                     (#:pp pp-contract
                      #:racket-colors? boolean?
                      #:post-process (-> (is-a?/c pasteboard%) any))
                     any)]
 
 [term-node? (-> any/c boolean?)]
 [term-node-parents (-> term-node? (listof term-node?))]
 [term-node-children (-> term-node? (listof term-node?))]
 [term-node-labels (-> term-node? (listof (or/c false/c string?)))]
 [term-node-set-red! (-> term-node? boolean? void?)]
 [term-node-set-color! (-> term-node? 
                           (or/c string? (is-a?/c color%) false/c)
                           void?)]
 [term-node-color (-> term-node? (or/c string? (is-a?/c color%) false/c))]
 [term-node-expr (-> term-node? any)]
 [term-node-set-position! (-> term-node? real? real? void?)]
 [term-node-x (-> term-node? real?)]
 [term-node-y (-> term-node? real?)]
 [term-node-width (-> term-node? real?)]
 [term-node-height (-> term-node? real?)]
 
 [stepper
  (->* (reduction-relation?
        any/c)
       (pp-contract)
       void?)]
 [stepper/seed 
  (->* (reduction-relation?
        (cons/c any/c (listof any/c)))
       (pp-contract)
       void?)]
 
 [dark-pen-color (parameter/c (or/c string? (is-a?/c color%)))]
 [light-pen-color (parameter/c (or/c string? (is-a?/c color%)))]
 [dark-brush-color (parameter/c (or/c string? (is-a?/c color%)))]
 [light-brush-color (parameter/c (or/c string? (is-a?/c color%)))]
 [dark-text-color (parameter/c (or/c string? (is-a?/c color%)))]
 [light-text-color (parameter/c (or/c string? (is-a?/c color%)))]
 [initial-font-size (parameter/c number?)]
 [initial-char-width (parameter/c (or/c number? (-> any/c number?)))])

(provide/contract 
 [reduction-steps-cutoff (parameter/c exact-nonnegative-integer?)]
 [default-pretty-printer
   (-> any/c output-port? exact-nonnegative-integer? (is-a?/c text%) 
       void?)]
 [pretty-print-parameters (parameter/c (-> (-> any) any))])
