#lang slideshow

(require racket/draw)

(define DELTA 80)
(define FT 12)

(define txt 
  '("(big-bang World_0"
    "  (to-draw render WIDTH HEIGHT)"
    "  (on-tick tock RATE)"
    "  (on-mouse click)"
    "  (on-key react)"
    "  (stop-when done))"
    ))

(define program 
  (apply vl-append (map (lambda (t) (text t '() (- FT 2))) txt)))

(define Program 
  (cc-superimpose 
   (rectangle (+ 5 (pict-width program)) (+ 5 (pict-height program)))
   program))

(define (make-state txt)
  (define t (text txt '() FT))
  (define e (rounded-rectangle (+ 10 (pict-width t)) (+ DELTA (pict-height t))))
  (cc-superimpose t e))

(define False (text "FALSE" '() FT))
(define True  (text "TRUE"  '() FT))
(define BOOL  (rectangle (+ 5 (pict-width False)) (+ 5 (pict-height False))))

;; String Boolean -> Pict 
(define (make-state0 txt b)
  ;; create the basic state
  (define t (text txt '() FT))
  (define s (if b 
                (cc-superimpose 
                 (rounded-rectangle (+ 5 (pict-width t))  (+ (- DELTA 5) (pict-height t)))
                 t)
                t))
  (define w 
    (cc-superimpose 
     s
     (rounded-rectangle (+ 10 (pict-width t)) (+ DELTA (pict-height t)))))
  ;; add the boolean 
  (define bb (cc-superimpose (if b True False) BOOL))
  (define ar (add-labeled-arrow (vc-append DELTA bb w) w ct-find bb cb-find "done"))
  (define scene (text "Scene" '() FT))
  (define sc (cc-superimpose scene (rectangle (+ 20 (pict-width scene)) (+ 30 (pict-height scene)))))
  (define br (add-labeled-arrow (vc-append DELTA ar sc) ar cb-find sc ct-find "render"))
  br)

(define (add-labeled-arrow nx locked lb-find closed lt-find txt)
  (define-values (x0 y0) (lb-find nx locked))
  (define-values (x1 y1) (lt-find nx closed))
  (define lbl (text txt '() (- FT 2)))
  (define wlbl (pict-width lbl))
  (define hlbl (pict-height lbl))
  (define x (- x0 (/ wlbl 2)))
  (define y (+ y0 (/ ( - y1 y0 hlbl) 2)))
  (pin-over (pin-arrow-line 4.0 nx locked lb-find closed lt-find) x y lbl))

(define (h-labeled-arrow t)
  (define tock (text t '() (- FT 2)))
  (define blk (blank (+ DELTA 4) 2))
  (vc-append tock (pin-arrow-line 4.0 blk blk lc-find blk rc-find)))

(define arrows
  (vc-append (h-labeled-arrow "tock")
             (h-labeled-arrow "click")
             (h-labeled-arrow "react")))

(define state0 (make-state0 "World_0" #f))
(define state1 (make-state0 "World_1" #f))
(define dots   (cc-superimpose (blank (pict-width state1) (pict-height state1)) (text "..." '() FT)))
(define state2 (make-state0 "World_N-1" #f))
(define stateN (make-state0 "World_N" #t))
(define states (list state0 arrows state1 arrows dots arrows state2 arrows stateN))

(define bg (blank (+ (apply + (map pict-width states))
                         DELTA #;(* (length states) DELTA))
                      (+ (pict-height state0) DELTA)))

(define (center base state x)
  (define w (pict-height state))
  (define d (floor (/ (- width w) 2)))
  (pin-over base x d state))

(define width (pict-height bg))

(define x (* 1/2 DELTA))
(define xx 
  (foldl (lambda (f ls s)
           (define y (center s f x))
           (set! x (+ x ls))
           y)
         bg
         states
         (map (lambda (x) (+ (pict-width x) #;(* 1/1 DELTA))) states)))

(define the-image (ct-superimpose xx Program))

(define image-bm
  (make-object bitmap% 
    (inexact->exact (round (pict-width the-image)))
    (inexact->exact (round (pict-height the-image)))))

(send image-bm ok?)

(define image-dc
  (new bitmap-dc% [bitmap image-bm]))
(send image-dc clear)

(draw-pict the-image image-dc 0.0 0.0)

(send image-bm save-file "nuworld.png" 'png)

the-image
