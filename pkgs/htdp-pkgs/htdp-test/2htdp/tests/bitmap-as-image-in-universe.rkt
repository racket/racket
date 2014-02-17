#lang racket

(require pict (only-in 2htdp/universe big-bang on-tick to-draw) rackunit)

(check-equal?
 103 
 (big-bang 100
           [on-tick add1 1/28 3]
           [to-draw (lambda (w) (pict->bitmap (circle w)))]))
