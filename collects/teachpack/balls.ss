;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "world.ss" "htdp"))

;; constants 
(define height 50)
(define delta  80)
(define width  (+ delta (* 2 height)))

(define left (quotient height 2))
(define right (+ height delta left))

;; World = (make-posn left Number) | (make-posn right Number)

(define server (text "server" 11 'black))
(define server* (overlay server (nw:rectangle (image-width server) (image-height server) 'outline 'black)))

;; visual constants 
(define bg
  (place-image 
   (text "universe" 11 'green)
   60 0
  (place-image 
   server*
   (+ height 15) 20
   (place-image 
    (text "left" 11 'blue)
    10 10
    (place-image
     (text "right" 11 'red)
     (+ height delta 10) 10
     (place-image
      (nw:rectangle delta height 'solid 'white)
      height 0 
      (place-image 
       (nw:rectangle width height 'solid 'gray)
       0 0
       (empty-scene width height))))))))

(define ball (circle 3 'solid 'red))

;; World -> Scene 
(define (draw w)
  (place-image ball (posn-x w) (posn-y w) bg))


;; World -> World 
(define (tick w)
  (local ((define y (posn-y w))
          (define x (posn-x w)))
    (cond
      [(> y 0) (make-posn x (- y 1))]
      [(= x left) (make-posn right height)]
      [(= x right) (make-posn left height)])))

(big-bang width height 1/66 (make-posn left height) true)
(on-redraw draw)
(on-tick-event tick)
