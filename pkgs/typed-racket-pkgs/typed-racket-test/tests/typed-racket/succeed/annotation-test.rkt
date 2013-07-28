#lang typed-scheme
(define-type-alias top2 Any)

(define: (x) : Number 23)

(let: ([y : top2 x])
      y)

(let: ([z : Number 4])
      #{z :: top2})

#{(x) :: top2}
