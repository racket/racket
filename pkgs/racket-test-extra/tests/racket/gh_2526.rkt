#lang racket/base

(require racket/generic
         racket/set
         rackunit)

(struct my-set (set)
  #:transparent
  #:methods gen:set
  [(define/generic super:set-member? set-member?)
  (define (set-member? this v)
    (super:set-member? (my-set-set this) v))
  (define/generic super:set-add set-add)
  (define (set-add this v)
    (super:set-add (my-set-set this) v))
  (define/generic super:set-remove set-remove)
  (define (set-remove this v)
    (super:set-remove (my-set-set this) v))
  (define/generic super:set-first set-first)
  (define (set-first this)
    (super:set-first (my-set-set this)))
  (define/generic super:set-empty? set-empty?)
  (define (set-empty? this)
    (super:set-empty? (my-set-set this)))
  (define/generic super:set-copy-clear set-copy-clear)
  (define (set-copy-clear this)
    (super:set-copy-clear (my-set-set this)))])

(check-pred null? (set->list (my-set (set))))
(check-pred sequence? (in-set (my-set (set))))
(check-pred sequence? (my-set (set)) "custom set is a sequence")
