#;#;
#<<END
TR opt: known-length-lists.rkt 41:0 (length l) -- known-length list length
TR opt: known-length-lists.rkt 42:0 (list-ref l i) -- known-length list op
TR opt: known-length-lists.rkt 43:0 (list-ref l j) -- known-length list op
TR opt: known-length-lists.rkt 44:0 (list-ref l k) -- known-length list op
TR opt: known-length-lists.rkt 45:0 (list-tail l i) -- known-length list op
TR opt: known-length-lists.rkt 46:0 (list-tail l j) -- known-length list op
TR opt: known-length-lists.rkt 47:0 (list-tail l k) -- known-length list op
TR opt: known-length-lists.rkt 48:0 (list-ref l 0) -- known-length list op
TR opt: known-length-lists.rkt 49:0 (list-ref l 1) -- known-length list op
TR opt: known-length-lists.rkt 50:0 (list-ref l 2) -- known-length list op
TR opt: known-length-lists.rkt 51:0 (list-tail l 0) -- known-length list op
TR opt: known-length-lists.rkt 52:0 (list-tail l 1) -- known-length list op
TR opt: known-length-lists.rkt 53:0 (list-tail l 2) -- known-length list op
END
#<<END
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

END

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
