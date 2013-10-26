#lang racket/base

;; Struct definitions for the profiler

;; An encapsulation of an analyzed profile call graph:
;; - total-time: the total time observed in msec (this is generally different
;;   than the time it took to run the profile).
;; - sample-number: the number of samples taken.
;; - thread-times: a list of (<thread-id> . msec) for the time spent in
;;   observed threads.
;; - nodes: the list of call-graph nodes sorted by their total time.
;; - *-node: a special node that is connected as a "caller" for all toplevel
;;   functions and a "callee" for all leaf functions.  It will also be
;;   identifiable by having both id and src fields being #f.  Can be used to
;;   start a graph traversal from the top or the bottom.
(provide (struct-out profile))
(struct profile
  (total-time cpu-time sample-number thread-times nodes *-node))

;; An entry for a single profiled function:
;; - id, src: the corresponding values from `continuation-mark-set->context'.
;; - thread-ids: the list of thread identifiers this function has been seen in.
;; - total: total msecs it participated in (= time in it, including callees).
;; - self: msecs where it was at the top of the stack (= time in its own code).
;; - callers, callees: a list of `edge' values for the time spent while it was
;;   called by the repective <node>, or it called it, sorted in decreasing msec
;;   time.
;; Note that the sum of caller/callee edges including the special `*-node'
;; should be equal to the `total' time.  So the edge from/to the `*-node' can
;; be used to get the time spent as a leaf or as a root divided by the number
;; of time the function appeared on the stack: so this value can be displayed
;; in the call-graph and the numbers will sum up nicely to a 100%.
(provide (struct-out node))
(define-struct node (id src thread-ids total self callers callees)
  #:mutable
  #:property prop:custom-write
  (λ (node o w?)
    (fprintf o "#<node:~s>"
             (or (node-id node) (if (node-src node) '??? 'ROOT)))))

;; An edge representing function calls between two nodes:
;; - total: the total time spent while the call was anywhere on the stack.
;; - caller, callee: the two relevant `node' values.
;; - caller-time, callee-time: the time that the caller/callee spent in this
;;   call relative to the callee/caller (different from the above time because
;;   each stack sample's time is divided by the number of times the
;;   caller/callee appears in that slice).
(provide (struct-out edge))
(define-struct edge (total caller caller-time callee callee-time)
  #:mutable
  #:property prop:custom-write
  (λ (edge o w?)
    (fprintf o "#<edge:~s-~s>"
             (or (node-id (edge-caller edge)) '???)
             (or (node-id (edge-callee edge)) '???))))
