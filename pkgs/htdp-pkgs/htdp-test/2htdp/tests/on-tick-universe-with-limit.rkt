#lang racket 

(require 2htdp/universe 2htdp/image)

(universe 0 
          (on-tick (lambda (w) (make-bundle (add1 w) '() '())) 1/28 3)
          (on-msg (lambda (w sender msg) (make-bundle w '() '())))
          (on-new cons))
