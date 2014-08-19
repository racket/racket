#lang scheme/base

(require 2htdp/universe
         "test-aux.rkt"
         (prefix-in 2: 2htdp/image)
         (prefix-in 1: htdp/image))

(define (see-full-rectangle x f)
  (big-bang x
            (on-tick sub1)
            (stop-when zero?)
            (on-draw (λ (x) (f 100 100 'outline 'black)))))

(testing 
  (see-full-rectangle 3 2:rectangle)
  (see-full-rectangle 3 1:rectangle))
