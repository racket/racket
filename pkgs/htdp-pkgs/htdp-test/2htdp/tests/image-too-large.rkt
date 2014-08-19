#lang racket

(require 2htdp/universe 2htdp/image "test-aux.rkt")

(define width 100000)
(define height 10)
(define image (rectangle width height 'solid 'red))
(define small (rectangle 100 100 'solid 'black))

(define (draw-large i)
  image)

(testing
  (check-true
    (with-handlers ([exn:fail? (lambda (x)
				 (define msg (exn-message x))
				 (define reg (regexp-match "draw-large" msg))
				 (pair? reg))])
      (big-bang 0 (to-draw draw-large) (on-tick add1) (stop-when zero?))
      #false))

  (check-true
    (with-handlers ([exn:fail? (lambda (x)
				 (define msg (exn-message x))
				 (define reg (regexp-match "to-draw" msg))
				 (pair? reg))])
      (big-bang 0
	(to-draw draw-large width height)
	(on-tick add1)
	(stop-when zero?))
      #false))

  (check-true
    (local ((define first-time #true))
      (big-bang 0
	(to-draw (lambda (_) (begin0 (if first-time small image) (set! first-time #false))))
	(on-tick add1)
	(stop-when zero?))
      #true)))

