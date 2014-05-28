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
  (abs-push n uninit s)
  #:context (define-metafunction))

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug3)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(application
    (proc-const (val val) (branch (loc-noclr 0) 'a 'b))
    'x
    (install-value 0 'y (boxenv 0 'z))))

(test small-counter-example)
