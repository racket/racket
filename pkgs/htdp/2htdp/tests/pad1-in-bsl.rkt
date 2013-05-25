;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pad1-in-bsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; must stay in BSL 

(require 2htdp/universe)
(require 2htdp/image)

(define (render x)
  (place-image (circle 3 'solid 'red) (+ 150 (real-part x)) (+ 150 (imag-part x)) (empty-scene 300 300)))

(define (sub1-i x) (- x 0+i))
(define (add1-i x) (+ x 0+i))

(big-bang 0+0i
          (to-draw render)
          (on-tick add1-i 1/28 50)
          (on-pad (pad-handler (up sub1-i) (down add1-i) (left sub1) (right add1))))
