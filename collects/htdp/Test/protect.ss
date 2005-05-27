(load "pingp.ss")
;; ---------------------------------------------------------------------------
;; TeachPack: pingp.ss
;; Language: Advanced

;; To test: uncomment the block at the bottom.
;; The file tests the function protect from pingp.ss.
;; The file is used to build protect-play.ss.

(define (test-go x) (void))


(define (test-go x)
  (protect (mk-balls 10)
	   ; (list (make-ball (make-posn 100 100) (make-speed 8 -16)))
	   move-balls
	   remove-balls-hit-paddle
	   remove-outside-balls
	   balls-posn))


;; Adapting the relevant functions from the pingp game
;; ---------------------------------------------------

;; move-in-box : ball number -> ball or #f (if the ball gets destroyed)
(define (move-in-box ball t)
  (case (bounces-off ball t)
    ((NORTH-SOUTH)
     (move-in-box
       (ns-bounce
	 (move-ball ball (ns-time-to-wall ball)))
       (- t (ns-time-to-wall ball))))
    ((PADDLE) #f)
    (else (move-ball ball t))))

;; bounces-off : ball number -> {'NORTH-SOUTH,'PADDLE,'none}
(define (bounces-off ball t)
  (cond
    ((<= (ns-time-to-wall ball) (min (ew-time-to-wall ball) t)) 'NORTH-SOUTH)
    ((<= (ew-time-to-wall ball) (min (ns-time-to-wall ball) t))
     (cond
       [(landed-on-paddle? (ball-posn (move-ball ball (ew-time-to-wall ball))))
	'PADDLE]
       [else 'none]))
    (else 'none)))


;; Dealing with collections of balls
;; ---------------------------------

;; mk-balls : natnum -> list-of-balls
(define (mk-balls a-nn)
  (cond
    ((zero? a-nn) null)
    (else
     (cons 
      (make-ball (make-posn (random-between FAR_WEST EAST) (random SOUTH))
		 (make-speed (random-between MIN-X-SPEED MAX-X-SPEED) 
		             (random-between MIN-Y-SPEED MAX-Y-SPEED)))
      (mk-balls (- a-nn 1))))))

;; random-between : int int -> int (randomly in betweeen he two inputs)
(define (random-between low high)
  (+ low (random (+ (abs low) (abs high)))))

;; move-balls : list-of-balls -> list-of-balls 
(define (move-balls loballs)
  (cond
    ((null? loballs) null)
    (else (cons (move-in-box (first loballs) 1) (move-balls (rest loballs))))))

;; remove-balls-hit-paddle : list-of-balls/#f -> list-of-balls 
;;  (those that hit paddle during a move or are outside after a move)
(define (remove-balls-hit-paddle loballs)
  (cond
    ((null? loballs) null)
    (else (cond
	    ((boolean? (first loballs))
	     (remove-balls-hit-paddle (rest loballs)))
	    (else
	     (cons (first loballs) (remove-balls-hit-paddle (rest loballs))))))))

;; remove-outside-balls : list-of-balls -> list-of-balls 
;;  (those that hit paddle during a move or are outside after a move)
(define (remove-outside-balls loballs)
  (cond
    ((null? loballs) null)
    (else (cond
	    ((inside? (first loballs))
	     (cons (first loballs) (remove-outside-balls (rest loballs))))
	    (else
	     (remove-outside-balls (rest loballs)))))))

;; inside? : ball -> boolean (is the ball inside the user-defined space)
(define (inside? aball)
  (and (<= FAR_WEST (posn-x (ball-posn aball)) EAST)
       (<= NORTH    (posn-y (ball-posn aball)) SOUTH)))

;; balls-posn : list-of-balls -> list-of-posn (a projection)
(define (balls-posn l)
  (cond
    ((null? l) null)
    (else (cons (ball-posn (first l)) (balls-posn (rest l))))))

;; the true extent of the space
(define FAR_WEST (* -1 EAST))

(define MIN-X-SPEED 05)
(define MAX-X-SPEED 15)
(define MIN-Y-SPEED 1)
(define MAX-Y-SPEED 4)

(test-go 'x)