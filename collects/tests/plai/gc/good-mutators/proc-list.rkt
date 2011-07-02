#lang plai/mutator
; Demonstrates garbage collection while a closure is on the stack.  A correct collector must ensure that the roots
; reachable from (make-adder 90) and (make-adder 200) -- that is, the values 90 and 200 that k is bound to -- do
; not get discarded.
(allocator-setup "../good-collectors/good-collector.rkt" 58)

(define (make-adder k)
  (lambda (n) (+ n k)))

(define proc-list
  (cons (make-adder 90)
        (cons (make-adder 200)
              empty)))

(test/value=? ((first proc-list) 7) 97)
(test/value=? ((first proc-list) 300) 390)
(test/value=? ((first (rest proc-list)) 73) 273)
