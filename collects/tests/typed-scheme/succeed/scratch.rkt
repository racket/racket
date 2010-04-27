#lang typed-scheme

(let: ((x : Any 3)) (if (number? x) (add1 x) 12))
(let: ((v : (Un Number Boolean) #f)) (if (boolean? v) 5 (+ v 1)))


