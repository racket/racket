#lang scheme
(require 2htdp/universe 2htdp/image "test-aux.rkt")

(define (slow)
  (let sloop ([n (expt 2 22)])
    (unless (zero? n)
      (sloop (- n 1)))))

(define (update-world w)
  (slow)
  (- w 1))

(define (render w)
  (circle 30 'solid (if (odd? w) 'red 'green)))

(testing 
  (big-bang 10
    (on-tick update-world)
    (on-draw render)
    (stop-when zero?))

  (printf "done\n"))
