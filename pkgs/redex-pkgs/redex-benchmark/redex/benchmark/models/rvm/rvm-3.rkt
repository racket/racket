#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "application slots not initialized properly")

;; applies in 3 places
(define-rewrite bug3
  (abs-push n not s)
  ==> 
  (abs-push n uninit s))

(define-rewrite bug3-jdg
  (abs-push n not sl)
  ==> 
  (abs-push n uninit sl))

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug3)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite (lib "redex/benchmark/models/rvm/verif-jdg.rkt") verif-jdg bug3 bug3-jdg)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(application
    (proc-const (val val) (branch (loc-noclr 0) 'a 'b))
    'x
    (install-value 0 'y (boxenv 0 'z))))

(test small-counter-example)
