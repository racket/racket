#lang typed-scheme

(define: x : (U Number #f) 1)
(if x #{x :: Number} 1)
(lambda () 1)
(lambda: ([y : Number]) (if #t y y))

(plambda: (a) ([y : Number]) (if y #t #f))
(plambda: (a) ([y : a]) y)
(plambda: (a) ([y : a]) y)
(plambda: () ([y : Boolean]) (if y #t #f))

#{(if #t #t #t) :: Boolean}

(let () 3)
(let ([x 1] [y 2]) x)
#{(let ([x 1] [y 2]) x) :: Number}
(let: ([x : Number 1] [y : Integer 2]) x)
#{(let: ([x : Integer 1] [y : Integer 2]) x) :: Integer}

#{(let*: ([x : Number 1] [x : Integer 2]) x) :: Integer}

