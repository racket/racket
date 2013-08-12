#lang racket

(require 2htdp/universe)
(require htdp/image)
(require test-engine/scheme-tests)

(define-struct posn (x y) #:transparent)

(define world1 (make-posn 50 50))
(define world-in (make-posn 100 100))
(define world-out (make-posn 250 250))

(define mt (empty-scene 500 500))
(define sq (rectangle 10 10 'solid 'black))

(define (draw a-world)
  (cond
    [(equal? a-world world1)
     (place-image (text "move mouse in to canvas" 11 'red) 10 10
                  (place-image sq (posn-x a-world) (posn-y a-world) mt))]
    [(equal? a-world world-in)
     (place-image (text "move mouse out of canvas" 11 'red) 10 10
                  (place-image sq (posn-x a-world) (posn-y a-world) mt))]
    [else
     (place-image sq (posn-x a-world) (posn-y a-world) mt)]))

(check-expect (mouse-handler 'w 100 100 "leave") (make-posn 250 250))

(define (mouse-handler w x y me)
  (cond
    [(string=? "button-down" me) w]
    [(string=? "button-up" me) w]
    [(string=? "drag" me) w]
    [(string=? "move" me) w]
    [(string=? "enter" me) world-in]
    [(string=? "leave" me) world-out]))

(define (out? w)
  (equal? world-out w))

(define (main w)
  (big-bang world1 (on-draw draw) (stop-when out?) (on-mouse mouse-handler)))

(test)

(main 0)
