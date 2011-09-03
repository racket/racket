#lang typed/racket

(ann (for/list ([#{i : Number} (in-list '(1 2 3))]) i) (Listof Number))
(ann (for/list: ([i : Number (in-list '(1 2 3))]) i) (Listof Number))

(+ (for/fold: ([acc : Number 0])
       ([i (in-list '(1 2 3))])
     (+ i acc))
   1)
