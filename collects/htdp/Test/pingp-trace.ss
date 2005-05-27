;; TeachPack: pingp.ss
;; Language: Advanced

;; This file tests the trace functions. It is not used anywhere else. 

;; Speed and its relationship to positions
;; ---------------------------------------
(define-struct speed (x y))

;; posn+ : posn vec -> vec 
(define (posn+ p v)
  (make-posn (+ (posn-x p) (speed-x v)) (+ (posn-y p) (speed-y v))))

;; posn*s : number vec -> posn
(define (posn*s f p)
  (make-speed (* f (speed-x p)) (* f (speed-y p))))

;; vec* : vec vec -> vec
(define (vec* v1 v2) 
  (make-speed (* (speed-x v1) (speed-x v2)) (* (speed-y v1) (speed-y v2))))

;; The ball representation and some basic primitives:
;; ---------------------------------------------------
(define-struct ball (posn speed))

;; make-direction : (speed -> num) X Y -> (ball -> (union X Y))
(define (make-direction access dir1 dir2)
  (lambda (ball)
    (cond
      ((< (access (ball-speed ball)) 0) dir1)
      ((> (access (ball-speed ball)) 0) dir2)
      (else (error 'make-direction "can't happen")))))

;; ns-direction : ball -> {'NORTH, 'SOUTH}
(define ns-direction (make-direction speed-y 'NORTH 'SOUTH))

;; ew-direction : ball -> {'EAST, 'WEST}
(define ew-direction (make-direction speed-x 'WEST 'EAST))

;; make-distance : (posn -> num) (ball -> sym) sym num num -> (ball -> num)
(define (make-distance direction access dir bound1 bound2)
  (lambda (ball)
    (if (eq? (direction ball) dir) 
	(- (access (ball-posn ball)) bound1)
	(- bound2 (access (ball-posn ball))))))

;; make-time : (ball -> num) (speed -> num) -> (ball -> number)
(define (make-time distance access)
  (lambda (ball) 
    (/ (distance ball) (abs (access (ball-speed ball))))))

;; ns-time-to-wall : ball -> number (time before ns wall is hit)
(define ns-time-to-wall 
  (make-time (make-distance ns-direction posn-y 'NORTH NORTH SOUTH) speed-y))

;; ew-time-to-wall : ball -> number (time before ew wall is hit)
(define ew-time-to-wall 
  (make-time (make-distance ew-direction posn-x 'WEST WEST EAST) speed-x))

;; Moving a Ball
;; -------------
;; move-in-box : ball number -> ball
(define (move-in-box ball t)
  (case (bounces-from ball t)
    ((NORTH SOUTH) (bouncing-move ns-bounce (ns-time-to-wall ball) t ball))
    ((EAST WEST) (bouncing-move ew-bounce (ew-time-to-wall ball) t ball))
    (else (move-ball ball t))))

;; bouncing-move : (ball -> ball) num num ball -> ball
(define (bouncing-move bounce t-bounce t ball)
  (move-in-box (bounce (move-ball ball t-bounce)) (- t t-bounce)))

;; bounces-from : ball number -> {'NORTH, 'SOUTH, 'EAST, 'WEST, 'none}
(define (bounces-from ball t)
  (cond
    ((<= (ns-time-to-wall ball) (min (ew-time-to-wall ball) t)) (ns-direction ball))
    ((<= (ew-time-to-wall ball) (min t (ns-time-to-wall ball))) (ew-direction ball))
    (else 'none)))

;; move : ball number -> ball
(define (move-ball ball t)
  (make-ball (posn+ (ball-posn ball) (posn*s t (ball-speed ball))) (ball-speed ball)))

;; make-bounce : speed -> (ball -> ball)
(define (make-bounce bounceV)
  (lambda (ball)
    (make-ball (ball-posn ball) (vec* (ball-speed ball) bounceV))))  

;; ns-bounce : ball -> ball
(define ns-bounce (make-bounce (make-speed 1 -1)))

;; ew-bounce-west : ball -> ball
(define ew-bounce (make-bounce (make-speed -1 1)))

;; mover : posn speed number -> posn
(define (mover p s t) 
  (posn+ p (posn*s t s)))

(trace-ball (make-ball (make-posn 100 100) (make-speed 8 -2)) ball-posn move-in-box 222)

(trace (make-posn 100 100) (make-speed 8 -2) mover 222)
