#lang scheme
(require (prefix-in uni: 2htdp/universe)
         (prefix-in uni: htdp/image))

(define (create-UFO-scene height)
  (uni:place-image UFO 50 height (uni:empty-scene 100 100)))

(define UFO
  (uni:overlay (uni:circle 10 'solid 'green)
               (uni:rectangle 40 4 'solid 'green)))

(uni:big-bang 0
              (uni:on-tick add1)
	      (uni:stop-when (lambda (y) (>= y 100)))
              (uni:on-draw create-UFO-scene))
