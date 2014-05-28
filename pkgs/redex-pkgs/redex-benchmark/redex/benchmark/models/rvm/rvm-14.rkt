#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "certain updates to initialized slots could break optimizer assumptions")

(define-rewrite bug14a
  ((where uninit (stack-ref n s_2))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest)
  #:exactly-once)

(define-rewrite bug14b
  ((side-condition (term (AND (same? ṽ_0 uninit) ellipsis)))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest ellipsis)
  #:exactly-once)

(define-rewrite bug14c
  ([(closure-intact? (box-nc ṽ_1 ellipsis) . rest1)
    . rest2]
   . rest)
  ==> 
  ([(closure-intact? (box-nc ṽ_1 ellipsis) . rest1)
    . rest2]
   [(closure-intact? (box-nc ṽ_1 ellipsis) (imm ṽ_2 ellipsis))
    (closure-intact? (ṽ_1 ellipsis) (ṽ_2 ellipsis))]
   . rest)
  #:context (define-metafunction)
  #:variables (rest rest1 rest2 ellipsis)
  #:exactly-once)

(define-rewrite/compose bug14 bug14a bug14b bug14c)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug14)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(let-one 'x
            (application (proc-const (val val) (loc 0))
                         (loc-noclr 2)
                         (install-value 2 'y 'z))))

(test small-counter-example)
