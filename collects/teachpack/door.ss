#lang slideshow

(define DELTA 40)
(define FT 12)

;  (fsa "unlock" "lock" "push" "tick")
(define (fsa L C O unlock lock push tick)
  (define (make-state txt)
    (define t (text txt '() FT))
    (define e (rounded-rectangle (+ 10 (pict-width t)) (+ 10 (pict-height t))))
    (cc-superimpose t e))
  
  (define locked (make-state L))
  (define closed (make-state C))
  (define open   (make-state O))
  
  (define bg (rectangle (+ (pict-width locked) (* 2 DELTA))
                        (+ (pict-height locked)
                           (pict-height closed)
                           (pict-height open)
                           (* 3 DELTA))))
  
  (define width (pict-width bg))
  
  (define (center base state y)
    (define w (pict-width state))
    (define d (quotient (round (- width w)) 2))
    (pin-over base d y state))
  
  (define nx 
    (center 
     (center
      (center
       bg locked (/ DELTA 2))
      closed
      (+ (/ DELTA 2) (pict-height locked) DELTA))
     open
     (+ (/ DELTA 2) DELTA (pict-height locked) DELTA (pict-height closed))))
  
  (define (add-labeled-arrow nx locked lb-find closed lt-find txt)
    (define-values (x0 y0) (lb-find nx locked))
    (define-values (x1 y1) (lt-find nx closed))
    (define lbl (text txt '() (- FT 2)))
    (define wlbl (pict-width lbl))
    (define hlbl (pict-height lbl))
    (define x (- x0 (/ wlbl 2)))
    (define y (+ y0 (/ ( - y1 y0 hlbl) 2)))
    (pin-over (pin-arrow-line 4.0 nx locked lb-find closed lt-find) x y lbl))
  
  (define l1 (add-labeled-arrow nx locked lb-find closed lt-find unlock))
  (define l2 (add-labeled-arrow l1 closed lb-find open lt-find push))
  (define l3 (add-labeled-arrow l2 open rt-find closed rb-find tick))
  (define l4 (add-labeled-arrow l3 closed rt-find locked rb-find lock))
  l4)

(fsa "locked" "closed" "open" "unlock" "lock" "push" "time")
(fsa "'locked" "'closed" "'open" "#\\u" "#\\l" "#\\space" "tick")
