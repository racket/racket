#lang racket

;; ---------------------------------------------------------------------------------------------------
;; testing launch many worlds/proc

(require 2htdp/universe 2htdp/image)

(define (aworld x c)
  (big-bang 10
            [to-draw (lambda (i) (text (number->string x) (+ 33 i) c))]
            [on-tick sub1]
            [stop-when zero?]))

(define (main)
  (apply launch-many-worlds/proc
         (build-list 20 (lambda (x) (lambda () (aworld (+ 10 x) (make-color 255 255 x)))))))

(main)
