#lang racket

(require graphics/turtles)

(provide regular-poly regular-polys radial-turtles spaced-turtles
         spokes spyro-gyra neato graphics-bexam sierp-size sierp sierp-nosplit
         koch-size koch-split koch-draw lorenz lorenz1 peano-size
         peano-position-turtle peano fern-size fern1 fern2 gapped-lines)

(define (regular-poly sides radius)
  (define theta (/ (* 2 pi) sides))
  (define side-len (* 2 radius (sin (/ theta 2))))
  (define (draw-sides n)
    (cond
      [(zero? n) (void)]
      [else
       (draw side-len)
       (turn/radians theta)
       (draw-sides (sub1 n))]))
  (tprompt (move radius)
           (turn/radians (/ (+ pi theta) 2))
           (draw-sides sides)))

(define (regular-polys sides s)
  (define (make-polys n)
    (cond
      [(zero? n) (void)]
      [else
       (regular-poly sides (* n 5))
       (make-polys (sub1 n))]))
  (make-polys sides))

(define (radial-turtles n)
  (cond
    [(zero? n) (void)]
    [else
     (split (turn/radians (/ pi (expt 2 (sub1 n)))))
     (radial-turtles (sub1 n))]))

(define (spaced-turtles n)
  (cond
    [(zero? n) (void)]
    [else
     (split (move (expt 2 (+ n 1))))
     (spaced-turtles (sub1 n))]))

(define (spokes)
  (radial-turtles 4)
  (spaced-turtles 5)
  (turn/radians (/ pi 2))
  (draw 10))

(define (spyro-gyra)
  (radial-turtles 4)
  (regular-poly 3 100))

(define (neato)
  (define (spiral d t)
    (cond
      [(<= 1 d)
       (draw d)
       (turn/radians t)
       (spiral (- d 1) t)]
      [else (void)]))
  (radial-turtles 4)
  (spiral 30 (/ pi 12)))

(define (graphics-bexam)
  (define (gb d)
    (cond
      [(<= d 3)
       (draw d)]
      [else
       (define new-d (/ d 3))
       (gb new-d)
       (turn/radians (- (/ pi 2)))
       (gb new-d)
       (turn/radians (/ pi 2))
       (gb new-d)
       (turn/radians (/ pi 2))
       (gb new-d)
       (turn/radians (- (/ pi 2)))
       (gb new-d)]))
  (define square-size (expt 3 5))
  (split (turn/radians (/ pi 2))
         (move square-size)
         (turn/radians (- (/ pi 2)))
         (move square-size)
         (turn/radians pi))
  (split (move square-size)
         (turn/radians (/ pi 2)))
  (gb square-size))

(define sierp-size 120)

(define (sierp distance)
  (define sqrt3 (sqrt 3))
  (define -2pi/3 (- 0 (/ (* 2 pi) 3)))
  (define pi/6 (/ pi 6))
  (define -5pi/6 (- 0 (/ (* 5 pi) 6)))
  (define pi/2 (/ pi 2))
  (define (engine distance)
    (unless (< distance 1)
      (define side-half (* distance sqrt3))
      (define side (* 2 side-half))
      (turn/radians -2pi/3)
      (move distance)
      (split (move distance)
             (turn/radians -5pi/6)
             (draw side)
             (turn/radians -5pi/6)
             (move distance)
             (turn/radians pi)
             (split (turn/radians -5pi/6)
                    (move side-half)
                    (turn/radians pi/6)))
      (engine (/ distance 2))))
  (move (* 2 distance))
  (turn/radians (/ (* 5 pi) 6))
  (draw (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (move (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (draw (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (turn/radians (/ pi 6))
  (move (* 2 distance))
  (turn/radians pi)
  (engine distance))

(define (sierp-nosplit distance)
  (define sqrt3 (sqrt 3))
  (define -2pi/3 (- 0 (/ (* 2 pi) 3)))
  (define pi/6 (/ pi 6))
  (define -5pi/6 (- 0 (/ (* 5 pi) 6)))
  (define pi/2 (/ pi 2))
  (define (engine distance)
    (unless (< distance 1)
      (define side-half (* distance sqrt3))
      (define side (* 2 side-half))
      (turn/radians -2pi/3)
      (move distance)
      (engine (/ distance 2))
      (move distance)
      (turn/radians -5pi/6)
      (draw side)
      (turn/radians -5pi/6)
      (move distance)
      (turn/radians pi)
      (engine (/ distance 2))
      (turn/radians -5pi/6)
      (move side-half)
      (turn/radians pi/6)
      (engine (/ distance 2))
      (move (- distance))))
  (move (* 2 distance))
  (turn/radians (/ (* 5 pi) 6))
  (draw (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (move (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (draw (* distance 2 (sqrt 3)))
  (turn/radians (/ (* 2 pi) 3))
  (turn/radians (/ pi 6))
  (move (* 2 distance))
  (turn/radians pi)
  (engine distance))

(define koch-size (expt 3 5))

(define (koch-split koch-size)
  (define (build-up-turtles n)
    (cond
      [(<= n 3) 'built]
      [else 
       (define third (/ n 3))
       (split* 'stay-put
               (move (* 2 third))
               (begin (move third)
                      (turn/radians (- (/ pi 3))))
               (begin (move third)
                      (turn/radians (- (/ pi 3)))
                      (move third)
                      (turn/radians (* 2 (/ pi 3)))))
       (build-up-turtles third)]))
  (split* 'stay-put
          (begin (move koch-size)
                 (turn/radians (/ (* 2 pi) 3)))
          (begin (turn/radians (/ pi 3))
                 (move koch-size)
                 (turn/radians pi)))
  (build-up-turtles koch-size)
  (draw 3))

(define (koch-draw koch-size)
  (define (side n)
    (cond
      [(<= n 3) (draw n)]
      [else 
       (define third (/ n 3))
       (side third)
       (turn/radians (- (/ pi 3)))
       (side third)
       (turn/radians (* 2 (/ pi 3)))
       (side third)
       (turn/radians (- (/ pi 3)))
       (side third)]))
  (split* 'stay-put
          (begin (move koch-size)
                 (turn/radians (/ (* 2 pi) 3)))
          (begin (turn/radians (/ pi 3))
                 (move koch-size)
                 (turn/radians pi)))
  (side koch-size))

(define (lorenz a b c)
  (define (loop x y z)
    (define delta 0.01)
    (define dx (* delta (* a (- y x))))
    (define dy (* delta (- (* x b) y (* x z))))
    (define dz (* delta (- (* x y) (* c z))))
    (draw-offset dx dz)
    (sleep 0.05)
    (erase-offset (- dx) (- dz))
    (move-offset dx dz)
    (loop (+ x dx)
          (+ y dy)
          (+ z dz)))
  (loop 1 1 1))

(define (lorenz1) (lorenz 50 60 11))

(define peano-size (expt 3 6))
(define (peano-position-turtle)
  (clear)
  (move -270)
  (turn/radians (/ pi 2))
  (move 250)
  (turn/radians (- (/ (* 3 pi) 4))))

(define (peano l)
  (cond
    [(<= l 3)
     (draw l)]
    [else
     (define new-l (/ l 3))
     (peano new-l)
     (tprompt (peano new-l)
              (split* (turn/radians (/ pi 2))
                      (turn/radians (- (/ pi 2))))
              (peano new-l))
     (tprompt (split* (turn/radians (/ pi 2))
                      (turn/radians (- (/ pi 2))))
              (peano new-l))
     (tprompt (split* (move new-l)
                      (begin (turn/radians (/ pi 2))
                             (move new-l)
                             (turn/radians (- (/ pi 2))))
                      (begin (turn/radians (- (/ pi 2)))
                             (move new-l)
                             (turn/radians (/ pi 2))))
              (peano l))
     (move (* 2 new-l))]))

(define fern-size 30)

(define (fern1 n)
  (cond
    [(< 1 n)
     (draw (/ n 2))
     (tprompt (split* (turn/radians (/ pi 3))
                      (turn/radians (- (/ pi 3))))
              (fern1 (/ n 2)))
     (draw (/ n 2))
     (turn/radians 0.08)
     (fern1 (- n 1))]
    [else (void)]))


;; need to backup a little for this one.
(define (fern2 n)
  (define d 0.04)
  (define (fernd n sign)
    (cond
      [(< 1 n)
       (draw (/ n 2))
       (tprompt (turn/radians (/ pi 3))
                (fernd (/ n 2) -))
       (tprompt (turn/radians (- (/ pi 3)))
                (fernd (/ n 2) +))
       (draw (/ n 2))
       (turn/radians (sign d))
       (fernd (- n 1) sign)]
      [else (void)]))
  (fernd n +))

(define (gapped-lines)
  (define gaps 5)
  (define lines 3)
  (tprompt
   (turn/radians (/ pi 2))
   (spaced-turtles lines)
   (turn/radians (- (/ pi 2)))
   (draw (* 4 (expt 2 gaps))))
  (tprompt
   (spaced-turtles gaps)
   (turn/radians (/ pi 2))
   (erase (* 4 (expt 2 lines)))))
