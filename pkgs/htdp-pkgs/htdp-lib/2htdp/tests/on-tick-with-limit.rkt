#lang racket 

(require 2htdp/universe 2htdp/image)

(big-bang 0 
          (on-tick add1 1/28 3)
          (to-draw (lambda (w) (circle (- 100 w) 'solid 'red))))
