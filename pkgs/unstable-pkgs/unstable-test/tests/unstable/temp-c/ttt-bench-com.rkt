#lang racket/base
(provide player)

(define ((player mark) b board-ref board-set)
  (for*/or ([r (in-range 3)]
            [c (in-range 3)])
    (if (board-ref b r c)
        #f
        (board-set b r c mark))))
