#lang scheme/base

;; scheme/promise has srfi-45-style primitives
(require scheme/promise)
(provide (all-from-out scheme/promise))

;; TODO: there is a small difference between the primitives in srfi-45 and the
;; ones provided by scheme/promise (the latter is a bit more permissive).  See
;; "library approach" in scheme/promise and see the post-finalization
;; discussion on the srfi-45 list.  I (Eli) showed at some point how the
;; "language approach" primitives can be used to implement the other, and this
;; needs to be done here too.
