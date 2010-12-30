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
(define badtricirc
  (overlay/align "middle" "middle"
                 TRI
                 CIRC))

(define (rotate-1 pic)
  (rotate 1 pic))

"Triangle rotating around its center:"
(big-bang tricirc
          (on-tick rotate-1 .05)
          (check-with image?)
          (on-draw show-it))

"Triangle rotating around the center of its bounding box:"
(big-bang badtricirc
	  (on-tick rotate-1 .05)
	  (check-with image?)
	  (on-draw show-it))
