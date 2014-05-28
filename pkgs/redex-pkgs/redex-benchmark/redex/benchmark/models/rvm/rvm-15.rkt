#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "neglected to restrict case-lam to accept only 'val' arguments")

(define-rewrite bug15v
  ((verify (case-lam (name l (lam (val ellipsis1) (n ellipsis2) e)) ellipsis3) s n_l b γ η f)
   . rest)
  ==>
  ((verify (case-lam (name l (lam (τ ellipsis1) (n ellipsis2) e)) ellipsis3) s n_l b γ η f)
   . rest)
  #:context (define-metafunction)
  #:variables (rest ellipsis1 ellipsis2 ellipsis3)
  #:exactly-once)

(define-rewrite bug15rt
  `(case-lam ,@(map (curry recur depth #f) ls))
  ==>
  `(case-lam ,@(map (curry recur depth #t) ls))
  #:context (match)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug15v)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw bug15rt)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(let-one 42 
            (boxenv 0 
                    (application (case-lam (lam (ref) () (loc-box 0))) 
                                 (loc-box 1)))))

(test small-counter-example)
