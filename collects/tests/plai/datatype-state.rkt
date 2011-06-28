#lang plai
(require (prefix-in eli: tests/eli-tester))

(define-type A (a (x number?)))

(define an-a (a 10))

(eli:test
 (a-x an-a) => 10
 (set-a-x! an-a 20)
 (a-x an-a) => 20)
