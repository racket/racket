#lang typed-scheme

(define: x : (U Number #f) 1)
(if x #{x :: Number} 1)
(lambda () 1)
(lambda: ([y : Number]) (if #t y y))

(plambda: (a) ([y : Number]) (if y #t #f))
(plambda: (a) ([y : a]) y)
(plambda: (a) ([y : a]) y)
(plambda: () ([y : Boolean]) (if y #t #f))
