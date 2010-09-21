;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flight-lander) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
;; DATA:
;; The World is a number, representing the x-position of the plane on the screen

(define PLANE (circle 100 "solid" "black"))  

;; WIDTH of BACKGROUND
(define WIDTH 800)

;; HEIGHT of BACKGROUND
(define HEIGHT 500)

;; PLANE-MOVE-X: the amount that the plane should move horizontally per tick.
(define PLANE-MOVE-X 20)

;; BASE-HEIGHT of the ground.
(define BASE-HEIGHT 50)

;; WIDTH of the water.
(define WATER-WIDTH 500)

;; WATER: image
(define WATER (nw:rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue"))

;; LAND: image
(define LAND (nw:rectangle (- WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown"))

;; BACKGROUND: scene
(define BACKGROUND
  (place-image WATER
               0
               (- HEIGHT BASE-HEIGHT)
               (place-image LAND
                            WATER-WIDTH
                            (- HEIGHT BASE-HEIGHT)
                            (empty-scene WIDTH HEIGHT))))   

;; FUNCTIONS:

;; move-plane-x-on-tick: world -> world
;; increase the x-position of PLANE by PLANE-MOVE-X
(define (move-plane-x-on-tick x)
  (+ x PLANE-MOVE-X))

;; TEST:
(check-expect (move-plane-x-on-tick 10) (+ 10 PLANE-MOVE-X))
(check-expect (move-plane-x-on-tick 0) (+ 0 PLANE-MOVE-X))
(check-expect (move-plane-x-on-tick 800) (+ 800 PLANE-MOVE-X))

;; place-plane-x: world -> Scene
;; place PLANE whose x-posn is x onto BACKGROUND
(define (place-plane-x x)  
  (place-image PLANE 
               x 
               50 
               BACKGROUND))

;; RUN PROGRAM 
(big-bang WIDTH HEIGHT 1/30 0)
(on-tick-event move-plane-x-on-tick)
(on-redraw place-plane-x)