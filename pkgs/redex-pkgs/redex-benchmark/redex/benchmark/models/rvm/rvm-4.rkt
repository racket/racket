#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "mishandling branches when then branch needs more stack than else branch; bug in the boxenv case not checking a stack bound")

(define-rewrite bug3
  ((side-condition (< (term n_p) (term n_l)))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest)
  #:exactly-once)

(define-rewrite bug4-jdg
  [(V (boxenv n_p e) s n_l b γ η f s_2 γ_2 η_2)
    (where imm (sref n_p s))
    (V e (supdt n_p box s) n_l b γ η f s_2 γ_2 η_2)
    (n< n_p n_l)]
  ==> 
  [(V (boxenv n_p e) s n_l b γ η f s_2 γ_2 η_2)
    (where imm (sref n_p s))
    (V e (supdt n_p box s) n_l b γ η f s_2 γ_2 η_2)]
  #:context (define-judgment-form)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug3)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite (lib "redex/benchmark/models/rvm/verif-jdg.rkt") verif-jdg bug4-jdg)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(let-one 'x
            (branch #f (boxenv 0 'y) (loc-box 0))))

(test small-counter-example)
