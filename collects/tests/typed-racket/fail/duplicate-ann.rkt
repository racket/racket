#lang scheme/load

(module square typed-scheme

  ;(provide: [square (Integer -> Integer)])
  (provide: [square (Integer -> Integer)])
  ;(: square (Number -> Number))
  (define: (square [n : Number]) : Number
    (* n n))
  )

(module squareclient typed-scheme

  (require 'square)

  (square 10)  ;; 100
  (integer? 10.1)  ;; #f
  (square 10.1)  ;; 102.009999...
  )
