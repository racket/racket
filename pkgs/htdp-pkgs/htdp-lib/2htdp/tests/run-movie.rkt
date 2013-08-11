#lang racket

(require 2htdp/universe 2htdp/image)

(define (make-images i)
  (cond
    [(zero? i) '()]
    [else (cons (place-image DOT 50 (* 50 i) BACKGROUND) (make-images (sub1 i)))]))

(define DOT (circle 3 'solid 'red))
(define BACKGROUND (empty-scene 100 400))

(run-movie 1/8 (make-images 8))
