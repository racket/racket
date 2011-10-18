;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require htdp/image)

;; testing world
;; World = Nat

(define world0 100)

(define (world->image w)
  (place-image (circle 3 'solid 'red) 50 w (empty-scene 100 100)))

(define (world->next w) (sub1 w))

(define (world->steer w ke)
  (cond
    [(char? ke) w]
    [(symbol=? ke 'left) 100]
    [(symbol=? ke 'right) 90]
    [else w]))

;; --- 
(check-expect (key-event? 'a) false)
(check-expect (key-event? 0) false)
(check-expect (key-event? "a") true)

(check-expect (key=? "b" "a") false)

(check-error (key=? "a" 0) "key=?: expects a KEY-EVTS as second argument, given 0")

;; run world run

(define (main world0)
  (big-bang world0
            (on-draw world->image)
            (on-tick world->next)
            (on-key world->steer)
            (stop-when zero?)))
