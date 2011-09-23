#;
(exn-pred 1)

#lang typed/racket

(define-struct: node ({left : Tree} {right : Tree}) #:transparent)
(define-type Tree (Rec Tree (U Number Tree)))
;; Tree = Number | (node Tree Tree)

(: tree-sum (Tree -> Number))
;; sum up all numbers in the tree
(define (tree-sum t)
  (cond
    [(number? t) t]
    [else (+ (tree-sum (node-left t)) (tree-sum (node-right t)))]))

(define n (node (node 12000000000000.1 .0000000000000000000000001) 2))

(tree-sum n)
