#lang racket/load

(require rackunit)

(module module0 racket/base
  (require racket/contract unstable/latent-contract)
  
  (provide identity listify)
  
  (define/latent-contract (identity x) (real? . -> . real?)
    x)
  (define/latent-contract (listify x) (parametric->/c [a] (a . -> . (listof a)))
    (list x)))

(module module1 racket/base
  (require 'module0 unstable/latent-contract)
  (provide (activate-contract-out identity listify)))

(require 'module0)

(check-true (procedure? identity))
(check-true (procedure? listify))
(check-equal? (identity 1) 1)
(check-equal? (identity 'x) 'x)
(check-equal? (listify 1) (list 1))

(require 'module1)

(check-true (procedure? identity))
(check-true (procedure? listify))
(check-equal? (identity 1) 1)
(check-exn exn:fail:contract? (λ () (identity 'x)))
(check-equal? (listify 1) (list 1))
