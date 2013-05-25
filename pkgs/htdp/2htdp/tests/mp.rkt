#lang scheme (require test-engine/scheme-tests)
(require 2htdp/universe)
(require htdp/image)


;; WorldState = Image 

;; graphical constants
(define mt (empty-scene 100 100))

;; clack : WorldState Nat Nat String -> Worldstate 
;; add a dot at (x,y) to ws 

(check-expect 
 (clack mt 10 20 "button-down")
 (place-image (circle 1 "solid" "red") 10 20 mt))

(check-expect 
 (clack (place-image (circle 1 "solid" "red") 1 2 mt) 3 3 "button-down")
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

(test)

;; run program run 
(define (main x)
  (big-bang (empty-scene 100 100)
            (on-draw show)
            (record? x)
            (on-mouse clack)))

(main false)
