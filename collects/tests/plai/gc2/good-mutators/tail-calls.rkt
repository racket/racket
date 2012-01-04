#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 68)

(define (length-accum lst len)
  (if (empty? lst)
      len
      (length-accum (rest lst) (+ 1 len))))

(define (length lst)
  (length-accum lst 0))

(define (fact/acc n a)
  (if (zero? n)
      a
      (fact/acc (- n 1) (* n a))))

(test/value=? (length '(1 2 3 4)) 4)
(test/value=? (fact/acc 40 1) 815915283247897734345611269596115894272000000000)
