#lang racket/base
(require (for-syntax racket/base)
         racket/match
         unstable/struct
         "deriv.rkt")

(provide make

         Wrap
         
         ok-node?
         interrupted-node?

         wderiv-e1
         wderiv-e2
         wlderiv-es1
         wlderiv-es2
         wbderiv-es1
         wbderiv-es2

         wderivlist-es2)

;; Wrap matcher
;; Matches unwrapped, interrupted wrapped, or error wrapped
(define-match-expander Wrap
  (lambda (stx)
    (syntax-case stx ()
      [(Wrap S (var ...))
       (syntax/loc stx (struct S (var ...)))])))

;; ----

(define (check sym pred type x)
  (unless (pred x)
    (raise-type-error sym type x)))

(define (ok-node? x)
  (check 'ok-node? node? "node" x)
  (and (node-z2 x) #t))
(define (interrupted-node? x)
  (check 'interrupted-node? node? "node" x)
  (not (node-z2 x)))


(define (wderiv-e1 x)
  (check 'wderiv-e1 deriv? "deriv" x)
  (node-z1 x))
(define (wderiv-e2 x)
  (check 'wderiv-e2 deriv? "deriv" x)
  (node-z2 x))

(define (wlderiv-es1 x)
  (check 'wlderiv-es1 lderiv? "lderiv" x)
  (node-z1 x))
(define (wlderiv-es2 x)
  (check 'wlderiv-es2 lderiv? "lderiv" x)
  (node-z2 x))

(define (wbderiv-es1 x)
  (check 'wbderiv-es1 bderiv? "bderiv" x)
  (node-z1 x))
(define (wbderiv-es2 x)
  (check 'wbderiv-es2 bderiv? "bderiv" x))

;; wderivlist-es2 : (list-of WDeriv) -> (list-of Stx)/#f
(define (wderivlist-es2 xs)
  (let ([es2 (map wderiv-e2 xs)])
    (and (andmap syntax? es2) es2)))
