#lang slideshow

(require slideshow/pict)

(define DELTA 80)
(define FT 12)

(define initialize "register")
(define proc-msg "process")

(define program 
  (apply vl-append (map (lambda (t) (text t '() (- FT 2)))
                        (list (format "(universe ~a ~a)" initialize proc-msg)))))

(define Program 
  (cc-superimpose 
   (rectangle (+ 5 (pict-width program)) (+ 5 (pict-height program)))
   program))

;; String Boolean -> Pict 
(define (make-state0 txt b)
  ;; create the basic state
  (define t (text txt '() FT))
  (cc-superimpose t (rounded-rectangle (+ 10 (pict-width t)) (+ DELTA (pict-height t)))))

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
(define MessageI (vc-append (arrowhead 4 (* 3/2 pi)) (make-Message)))

(define M (rb-superimpose Message (blank DELTA  DELTA))) 
(define K (rb-superimpose MessageK (blank DELTA  DELTA)))
(define I (rb-superimpose MessageI (blank DELTA  DELTA)))

(define (make-arrows M lbl)
  (define Tock (h-labeled-arrow lbl))
  (values Tock (vc-append (blank DELTA (/ DELTA 2)) Tock M)))

(define-values (TockM arrowsR) (make-arrows M proc-msg))
(define-values (TockK arrowsL) (make-arrows K proc-msg))
(define-values (init  arrows) (make-arrows I initialize))

(define state0 (make-state0 "Server_0" #f))
(define state2 (make-state0 "Server_N-1" #f))
(define Univrs (hc-append (arrowhead 4 0) (cc-superimpose (cloud 160 80) (text "Universe" '() FT ))))
(define dots   (vc-append
                (blank (pict-width state2) (quotient (pict-height state2) 1))
                (text "..." '() FT) 
                (blank (pict-width state2) (* (pict-height state2)))
                Univrs))

(define states (list arrows
                     state0
                     arrowsL
                     dots
                     arrowsR
                     state2 
                     (h-labeled-arrow proc-msg)))

(define bg (blank (+ (apply + (map pict-width states)) DELTA) (pict-height dots)))

(define (center base state x)
  (define w (pict-height state))
  (define d (quotient (- (pict-height bg) w) 2))
  (pin-over base x d state))

(define x (* 1/2 DELTA))
(define xx 
  (foldl (lambda (f ls s)
           (define y (center s f x))
           (set! x (+ x ls))
           y)
         bg
         states
         (map pict-width states)))

(define zz (ct-superimpose xx Program))

(require mred/mred)

(define the-image
  (lt-superimpose 
   (dc (lambda (dc x y)
         (define-values (mx my) (cb-find zz MessageK))
         (define-values (tx ty) (ct-find zz MessageK))
         (define-values (ix iy) (ct-find zz MessageI))
         (define-values (jx jy) (cb-find zz MessageI))
         (define-values (sx sy) (lc-find zz Univrs))
         (define-values (tockx tocky) (lb-find zz TockK))
         (define-values (initx inity) (lb-find zz init))
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
         (set! dcp (make-object dc-path%))
         (set! cx (min sx jx))
         (set! cy (max sy jy))
         (send dc set-smoothing 'aligned)
         (send dcp move-to jx jy)
         (send dcp curve-to jx jy cx cy sx sy)
         (send dc draw-path dcp)
;; --- draw arc from Message to Receiver 
         (add-curve tockx tocky)
         (set! tx ix) (set! ty iy)
         (add-curve initx inity)
         ;; --- 
         dc)
       (pict-width zz) (pict-height zz))
   (lt-superimpose
    zz
    (dc (lambda (dc x y)
          (define-values (mx my) (cb-find zz Message))
          (define-values (tx ty) (ct-find zz Message))
          (define-values (sx sy) (rc-find zz Univrs))
          (define-values (tockx tocky) (rb-find zz TockM))
          (define (add-curve rx ry)
            (set! dcp (make-object dc-path%))
            (set! cx (min rx tx))
            (set! cy (min ry ty))
            (send dcp move-to tx ty)
            (send dcp curve-to tx ty cx cy rx ry)
            (send dc draw-path dcp))
          (define dcp (make-object dc-path%))
          ;; --- draw arc from Message to Server 
          (define cx (max sx mx))
          (define cy (max sy my))
          (send dc set-smoothing 'aligned)
          (send dcp move-to mx my)
          (send dcp curve-to mx my cx cy sx sy)
          (send dc draw-path dcp)
          ;; --- draw arc from Message to Receiver 
          (add-curve tockx tocky)
          ;; --- 
          dc)
        (pict-width zz) (pict-height zz)))))

(define image-bm
  (make-object bitmap% 
    (inexact->exact (round (pict-width the-image)))
    (inexact->exact (round (pict-height the-image)))))

(send image-bm ok?)

(define image-dc
  (new bitmap-dc% [bitmap image-bm]))
(send image-dc clear)

(draw-pict the-image image-dc 0.0 0.0)

(send image-bm save-file "server2.png" 'png)

the-image