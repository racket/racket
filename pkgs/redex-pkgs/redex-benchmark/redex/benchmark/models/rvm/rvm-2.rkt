#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "stack offset / pointer confusion")

(define-rewrite bug2
  [(redo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ellipsis1) s) 
   (redo-clrs ((n_1 ṽ_1) ellipsis2) (set not ,(- (- (term n_h) (term n_0)) (term 1)) s))
   (where n_h ,(length (term s)))
   (side-condition (< (term n_0) (term n_h)))]
  ==> 
  [(redo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ellipsis1) s) 
   (redo-clrs ((n_1 ṽ_1) ellipsis2) (set not ,(- (- (term n_h) (term n_0)) (term 1)) s))
   (where n_h ,(length (term s)))]
  #:context (define-metafunction)
  #:variables (ellipsis1 ellipsis2)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug2)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(proc-const (val)
               (branch (loc 0)
                       (let-one 'x
                                (branch (loc 1)
                                        (loc-clr 0)
                                        void))
                       void)))

(test small-counter-example)
