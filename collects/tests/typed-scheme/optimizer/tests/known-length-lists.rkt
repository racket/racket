#;
(
TR opt: known-length-lists.rkt 39:1 length -- known-length list length
TR opt: known-length-lists.rkt 40:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 41:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 42:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 43:1 list-tail -- known-length list op
TR opt: known-length-lists.rkt 44:1 list-tail -- known-length list op
TR opt: known-length-lists.rkt 45:1 list-tail -- known-length list op
TR opt: known-length-lists.rkt 46:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 47:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 48:1 list-ref -- known-length list op
TR opt: known-length-lists.rkt 49:1 list-tail -- known-length list op
TR opt: known-length-lists.rkt 50:1 list-tail -- known-length list op
TR opt: known-length-lists.rkt 51:1 list-tail -- known-length list op
'(1 2 3)
3
1
2
3
'(1 2 3)
'(2 3)
'(3)
1
2
3
'(1 2 3)
'(2 3)
'(3)
)

#lang typed/racket

(define i 0)
(define j 1)
(define: k : 2 2) ; otherwise will typecheck as Positive-Byte
(define l (ann '(1 2 3) (List Byte Byte Byte)))

(length l)
(list-ref l i)
(list-ref l j)
(list-ref l k)
(list-tail l i)
(list-tail l j)
(list-tail l k)
(list-ref l 0)
(list-ref l 1)
(list-ref l 2)
(list-tail l 0)
(list-tail l 1)
(list-tail l 2)
