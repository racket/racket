#lang typed-scheme
(define-typed-struct (a) bag ([val : a]))

(provide make-bag)

(let: ([x : (bag Number) (make-bag #{3 :: Number})]
       [y : (bag Boolean) (make-bag #{#t :: Boolean})])
      (+ 4 (bag-val x))
      (not (bag-val y)))
