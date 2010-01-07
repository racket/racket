;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require htdp/image)

;; WorldState = Image 

;; graphical constants
(define mt (empty-scene 100 100))

;; clack : WorldState Nat Nat String -> Worldstate 
;; add a dot at (x,y) to ws 

(check-expect 
 (clack mt 10 20 "something mousy")
 (place-image (circle 1 "solid" "red") 10 20 mt))

(check-expect 
 (clack (place-image (circle 1 "solid" "red") 1 2 mt) 3 3 "")
 (place-image (circle 1 "solid" "red") 3 3
              (place-image (circle 1 "solid" "red") 1 2 mt)))

(define (clack ws x y action)
  (if (string=? "button-down" action)
      (place-image (circle 1 "solid" "red") x y ws)
      ws))

;; show : WorldState -> WorldState 
;; just reveal the current world state 

(check-expect (show mt) mt)

(define (show ws)
  ws)

;; run program run 
(big-bang (empty-scene 100 100)
          (on-draw show)
          (record? true)
          (on-mouse clack))
