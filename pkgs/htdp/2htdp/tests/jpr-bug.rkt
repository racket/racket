;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname jpr-bug) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;; This program demonstrated that the idea of using default handlers (K) for 
;; absent mouse and key handlers was a horrible idea. The balls on his cannvas
;; just started jumping around when the mouse moved in. 

(require 2htdp/universe)
(require 2htdp/image)


(define (animation2)
  (local [(define SIZE 300)
          (define SCENE (rectangle SIZE SIZE 'outline "black"))
          (define dM 1)
          (define INIT 0)
          (define (suivant m)
            (+ m dM))
          (define (dessiner m)
            (place-image (circle m 'solid "red") (random SIZE) (random SIZE) SCENE))]
    (big-bang INIT
              (on-tick suivant 1)
              (on-draw dessiner SIZE SIZE))))

(animation2)
