#lang racket
(require racklog
         "lang/tutorial.rkt")

(module+ test
  (require rackunit)

  (check-equal? (%which () (parent 'john 'douglas))
                '())
  (check-equal? (%which () (parent 'john 'max))
                #f))
