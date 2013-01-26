#lang typed/racket

(define-type (Tree A) (U (Listof A) (node A)))
(struct: (A) node ([val : A]
                   [left : (Tree A)]
                   [right : (Tree A)]))

(: tree-set (All (A) (A (Tree A) -> (Tree A))))
(define (tree-set y t)
  (cond [(node? t)  (node y (node-left t) (node-right t))]
        [else  (list y)]))

