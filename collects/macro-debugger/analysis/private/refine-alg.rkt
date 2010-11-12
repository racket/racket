#lang racket/base

;; intern Def, Use?

;; A Def is (def sym resolved-module-path int)
(struct def (sym mod phase) #:prefab)

;; A Use is (use Def int)
;; the offset is (ref-phase - def-phase)
(struct use (def offset) #:prefab)

;; A resolved is path or symbol.

;; An import is (import resolved int)
(struct import (resolved offset))

;; ========

;; uses : hash[Use => #t]
;; reqs : hash[import => mpi]
;; keeps : hash[import => mpi]

#|

(define (refine uses reqs keeps)
  (unless (= (hash-count uses) 0)
    (direct-def-uses uses reqs keeps)
    (recur-on-imports uses reqs keeps)))

|#

(define (hash-choose h)
  (let ([i (hash-iterate-first h)])
    (and i (hash-iterate-value h i))))

#|
Algorithm for refining bypass modules

loop: set of references (id, phase), set of requires (mod, phase)
  for every reference DEFINED* in a require R
    mark that require R NEEDED and remove from set
    eliminate every reference provided by R
      (including re-provides)
  now every reference left is re-provided by some remaining require
  recur on imports of requires

DEFINED* : really, defined in this module OR imported from a "private" module.
|#


;; ====================

#|
Another algorithm

Put all requires in priority queue, with max-depth-to-kernel
priority...

|#
