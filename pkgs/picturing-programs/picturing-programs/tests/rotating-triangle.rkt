;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rotating-triangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

(define R 60)
(define SIDE (* R (sqrt 3)))
(define TRI (triangle SIDE "solid" "blue"))
(define CIRC (circle R "solid" "white"))
(define tricirc (overlay/xy TRI
                            (- (/ SIDE 2) R) 0
                            CIRC))
(define (rotate-1 pic)
  (rotate 1 pic))

;Triangle rotating by itself (with its top and left attached to the top and left of the window):
(define (test1 dummy)
(big-bang TRI
          (on-tick rotate-1 .05)
          (check-with image?)
          (on-draw show-it)))

;Triangle rotating around its center:
(define (test2 dummy)
(big-bang tricirc
          (on-tick rotate-1 .05)
          (check-with image?)
          (on-draw show-it)))

;show-on-yellow : image -> image
(define (show-on-yellow pic)
  (overlay pic (rectangle (* 2 R) (* 2 R) "solid" "yellow")))

;Triangle rotating around its center, on a yellow background:
(define (test3 dummy)
(big-bang tricirc
          (on-tick rotate-1 .05)
          (check-with image?)
          (on-draw show-on-yellow)))

"Triangle rotating by itself (with its top and left attached to the top
and left of the window): (test1 'blah)"
"Triangle rotating around its center: (test2 'blah)"
"Triangle rotating around its center, on a yellow background: (test3 'blah)"
