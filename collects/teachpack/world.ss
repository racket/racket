#lang slideshow

(define DELTA 80)
(define FT 12)

(define prgm 
  '("(big-bang World_0"
    "  (to-draw render WIDTH HEIGHT)"
    "  (on-tick tock RATE)"
    "  (on-mouse click)"
    "  (on-key react)"
    "  (on-receive receive)"
    "  (stop-when done)"
    "  (name 'jimbob)"
    "  (register LOCALHOST))"))


(define program 
  (apply vl-append (map (lambda (t) (text t '() (- FT 2))) prgm)))

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

(define message (text "Message" '() FT))
(define (make-Message)
  (cc-superimpose message (rectangle (+ 20 (pict-width message)) (+ 30 (pict-height message)))))

(define Message  (vc-append (make-Message) (arrowhead 4 (* 1/2 pi))))
(define MessageK (vc-append (arrowhead 4 (* 3/2 pi)) (make-Message)))

(define M (rb-superimpose Message (blank DELTA  DELTA))) 
(define K (rb-superimpose MessageK (blank DELTA  DELTA)))

(define (make-arrows M)
  (define Tock (h-labeled-arrow "tock"))
  (define Click (h-labeled-arrow "click"))
  (define Clack (h-labeled-arrow "react"))
  (define Receive (h-labeled-arrow "receive"))
  (values Tock Click Clack Receive (vc-append (blank DELTA (/ DELTA 2)) Tock Click Clack Receive M)))

(define-values (TockM ClickM ClackM ReceiveM arrowsR) (make-arrows M))
(define-values (TockK ClickK ClackK ReceiveK arrowsL) (make-arrows K))

(define state0 (make-state0 "World_0" #f))
(define state1 (make-state0 "World_1" #f))
(define Server (hc-append (arrowhead 4 0) (cc-superimpose (cloud 160 80) (text "SERVER" '() FT ))))
(define dots   (vc-append
                (cc-superimpose (blank (pict-width state1) (pict-height state1)) (text "..." '() FT))
                Server))
(define state2 (make-state0 "World_N-1" #f))
(define stateN (make-state0 "World_N" #t))
(define states (list state1 arrowsL dots arrowsR state2))

(define bg (blank (+ (apply + (map pict-width states)) DELTA)
                  (+ (pict-height state0) DELTA)))

(define (center base state x)
  (define w (pict-height state))
  (define d (quotient (round (- width w)) 2))
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

(define zz xx)

(require mred/mred)

(define the-image
  (ct-superimpose Program
  (lt-superimpose 
   (dc (lambda (dc x y)
         (define-values (mx my) (cb-find zz MessageK))
         (define-values (tx ty) (ct-find zz MessageK))
         (define-values (sx sy) (lc-find zz Server))
         (define-values (tockx tocky) (lb-find zz TockK))
         (define-values (clickx clicky) (lb-find zz ClickK))
         (define-values (clackx clacky) (lb-find zz ClackK))
         (define-values (rx ry) (lb-find zz ReceiveK))
         (define (add-curve rx ry)
           (set! dcp (make-object dc-path%))
           (set! cx (max rx tx))
           (set! cy (min ry ty))
           (send dcp move-to tx ty)
           (send dcp curve-to tx ty cx cy rx ry)
           (send dc draw-path dcp))
         (define dcp (make-object dc-path%))
         ;; --- draw arc from Message to Server 
         (define cx (min sx mx))
         (define cy (max sy my))
         (send dc set-smoothing 'aligned)
         (send dcp move-to mx my)
         (send dcp curve-to mx my cx cy sx sy)
         (send dc draw-path dcp)
         ;; --- draw arc from Message to Receiver 
         (add-curve tockx tocky)
         (add-curve clickx clicky)
         (add-curve clackx clacky)
         (add-curve rx ry)
         ;; --- 
         dc)
       (pict-width zz) (pict-height zz))
   (lt-superimpose
    zz
    (dc (lambda (dc x y)
          (define-values (mx my) (cb-find zz Message))
          (define-values (tx ty) (ct-find zz Message))
          (define-values (sx sy) (rc-find zz Server))
          (define-values (rx ry) (rb-find zz ReceiveM))
          (define dcp (make-object dc-path%))
          ;; --- draw arc from Message to Server 
          (define cx (max sx mx))
          (define cy (max sy my))
          (send dc set-smoothing 'aligned)
          (send dcp move-to mx my)
          (send dcp curve-to mx my cx cy sx sy)
          (send dc draw-path dcp)
          ;; --- draw arc from Message to Receiver 
          (set! dcp (make-object dc-path%))
          (set! cx (min rx tx))
          (set! cy (min ry ty))
          (send dcp move-to tx ty)
          (send dcp curve-to tx ty cx cy rx ry)
          (send dc draw-path dcp)
          ;; --- 
          dc)
        (pict-width zz) (pict-height zz))))))

(define image-bm
  (make-object bitmap% 
    (inexact->exact (round (pict-width the-image)))
    (inexact->exact (round (pict-height the-image)))))

(send image-bm ok?)

(define image-dc
  (new bitmap-dc% [bitmap image-bm]))
(send image-dc clear)

(draw-pict the-image image-dc 0.0 0.0)

(send image-bm save-file "universe.png" 'png)

the-image
