#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 400)
(print-only-errors #f)

(define lst (cons 1 (cons 2 (cons 3 empty))))
(test/value=? lst '(1 2 3))

(define (length lst)
  (if (empty? lst)
      0
      (add1 (length (rest lst)))))

(test/value=? (length '(hello goodbye)) 2)

(define tail (cons 1 empty))
(define head (cons 4 (cons 3 (cons 2 tail))))
(set-rest! tail head)
(test/location=? head (rest tail))
(test/location=? head tail)
