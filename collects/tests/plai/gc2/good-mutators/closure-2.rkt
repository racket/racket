#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)


(define make-conser
  (lambda (n)
    (lambda (x) 
      (cons n x))))


(define kons (make-conser 'catamaran))
;1 2 3 5 6 7 
(kons 'people)
(kons 'maroon)
(kons 'srfi)
(test/value=? (kons 'peace) '(catamaran . peace))
