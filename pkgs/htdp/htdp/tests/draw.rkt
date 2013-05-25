;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draw) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; This tests a good portion but needs some more 
;; This needs some tests for error behavior of functions ... 

;; draw-next-part : symbol -> true
;; consumes one of the seven body-part symbols and draws that part.

(define (draw-next-part body-part)
  (cond 
    [(eq? body-part 'body)
     (draw-solid-line (make-posn 100 60) (make-posn 100 130) 'black)]
    [(eq? body-part 'right-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 30 170) 'black)]
    [(eq? body-part 'left-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 170 170) 'black)]
    [(eq? body-part 'right-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 40 65) 'black)]
    [(eq? body-part 'left-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 160 65) 'black)]
    [(eq? body-part 'head)
     (draw-solid-disk (make-posn 100 50) 10 'black)]
    [(eq? body-part 'noose)
     (and
      (draw-solid-disk (make-posn 120 50) 30 'red)
      (draw-solid-line (make-posn 100 30) (make-posn 100 10) 'black)
      (draw-solid-line (make-posn 100 10) (make-posn 0 10) 'black)
      (draw-solid-line (make-posn 115 35) (make-posn 123 43) 'black)
      (draw-solid-line (make-posn 123 35) (make-posn 115 43) 'black)
      (draw-solid-line (make-posn 131 40) (make-posn 139 48) 'black)
      (draw-solid-line (make-posn 139 40) (make-posn 131 48) 'black))]))

;; ----- Tests -----------------------------------------------------------------

(check-expect (start 200 400) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'noose) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'head) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'left-arm) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'right-arm) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'body) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'left-leg) true)
(check-expect (sleep-for-a-while 1) true)
(check-expect (draw-next-part 'right-leg) true)
(check-expect (draw-solid-string (make-posn 10 200) "please click on the canvas") true)
(check-expect (posn? (wait-for-mouse-click)) true)
(check-expect (stop) true)
