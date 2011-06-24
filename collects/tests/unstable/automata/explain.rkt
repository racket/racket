#lang racket
(require racket/require 
         (path-up "automata/machine.rkt")
         (path-up "automata/re.rkt")
         (path-up "automata/re-ext.rkt")
         unstable/match
         tests/eli-tester)

(define r (re (seq 1 2 3)))
(define r0 (r 1))
(machine-accepting? (r0 1))
(machine-explain r0)