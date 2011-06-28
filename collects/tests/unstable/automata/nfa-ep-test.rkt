#lang racket/base
(require unstable/automata/nfa-ep
         unstable/automata/machine
         tests/eli-tester)

(define M
  (nfa/ep (s0) (s1 s3)
       [s0 ([epsilon (s1)]
            [epsilon (s3)])]
       [s1 ([0 (s2)]
            [1 (s1)])]
       [s2 ([0 (s1)]
            [1 (s2)])]
       [s3 ([0 (s3)]
            [1 (s4)])]
       [s4 ([0 (s4)]
            [1 (s3)])]))

(test
 (machine-accepts? M (list 1 0 1 0 1))
 (machine-accepts? M (list 0 1 0 1 0))
 (machine-accepts? M (list 1 0 1 1 0 1))
 (machine-accepts? M (list 0 1 0 0 1 0))
 (machine-accepts? M (list))
 (machine-accepts? M (list 1 0)) => #f)
