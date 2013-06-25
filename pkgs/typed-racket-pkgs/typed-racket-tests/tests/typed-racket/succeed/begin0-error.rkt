
#lang typed/racket

(define-type SN (U String Number))

(define-predicate sn? SN)

(struct: (α) node ({left : α} {right : α}))

(: create-node (Any Any -> String))
(define (create-node x y)
  (begin0
    "foo"
    (unless #t
      (error 'bad ""))))

(: create-node2 (All (B) (B B -> (node SN))))
(define (create-node2 x y)
  (if (and (sn? x) (sn? y))
      (node x y)
      (error 'bad "")))
