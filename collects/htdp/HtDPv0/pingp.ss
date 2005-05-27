#cs(module pingp mzscheme
     (require  "error.ss"
               "pingp-sig.ss"
               "big-draw.ss"
               (lib "unitsig.ss")
	       (lib "posn.ss" "lang"))

     (provide pingpU)

(define pingpU
  (unit/sig pingpS
    (import)

    ;; Exported Functions
    ;; ------------------

    ;; trace : posn S (posn S num -> posn) num -> #t
    (define (trace pos speed f e)
      ; --- error checking: redo
      (check-arg  'trace (posn? pos) 'posn '1st pos)
      ; the second arg is polymorphic
      (check-proc 'trace f 3 '3rd "3 arguments")
      (check-arg  'trace (and (number? e) (> e 0)) "positive number" '4th e)
      ; --- 
      (TT (make-bb pos speed)
	  bb-p
	  (lambda (b t) (let ((s (bb-s b))) (make-bb (f (bb-p b) s t) s)))
	  e))

    (define-struct bb (p s))

    ;; trace-ball : X (X -> posn) (X num -> X) number -> #t
    (define (trace-ball ball ball-posn f e)
      ; --- error checking
      ; the first arg is polymorphic
      (check-proc 'trace-ball ball-posn 1 '2nd "1 argument")
      (check-proc 'trace-ball f         2 '3rd "2 arguments")
      (check-arg  'trace-ball (and (number? e) (> e 0)) "positive number" '4th e)
      ; ---
      (TT ball ball-posn f e))

    ;; play : (posn X -> B) (num num -> S) (B -> posn) (B num -> B) -> #t
    (define (play make-ball make-speed ball-posn move)
      (check-proc 'play make-ball  2 '1st "2 arguments")
      (check-proc 'play make-speed 2 '2nd "2 arguments")
      (check-proc 'play ball-posn  1 '3rd "1 argument")
      (check-proc 'play move       2 '4th "2 arguments")
      (set-pad)
      (unset-trace)
      (play2 make-ball make-speed ball-posn move))

    ;; landed-on-paddle? : posn -> bool
    (define (%landed-on-paddle? x)
      (error 'landed-on-paddle? "can't happen"))
    (define (landed-on-paddle? aposn)
      (%landed-on-paddle? aposn))

    ;; change-speed : int[> 0] -> void
    (define (change-speed s)
      (check-arg 'change-speed (and (integer? s) (> s 0)) "positive integer" '1st s)
      (set! SLEEP (/ 10 s)))

    ;; change-wind : int[> 0] -> void
    (define (change-wind s)
      (check-arg 'change-wind (and (integer? s) (> s 0)) "positive integer" '1st s)
      (set! SWITCH s))

;    ;; change-width : int[> 200] -> void
;    (define (change-width s)
;      (check-arg 'change-width (and (integer? s) (> s 200)) "integer [> 200]" '1st s)
;      (set! EAST s))
;
;    ;; change-height : int[> 200] -> void
;    (define (change-height s)
;      (check-arg 'change-height (and (integer? s) (> s 200)) "integer [> 200]" '1st s)
;      (set! SOUTH s))

    (define NORTH 0)
    (define SOUTH 400)
    (define WEST  0)
    (define EAST  400)
    (define FAR-WEST (* -1 EAST))

    ;; protect : (listof ball)
    ;;           ((listof ball) -> (listof ball))
    ;;           ((listof ball) -> (listof ball))
    ;;           ((listof ball) -> (listof ball))
    ;;           ((listof ball) -> (listof posn))
    ;;           -> void
    (define (protect objs move-objs remove-objs-hit-paddle remove-outside-objs objs-posn)
      ; --- error checking
      (check-arg 'protect (list? objs) 'list        '1st objs)
      (check-proc 'protect move-objs              1 '2nd "1 argument")
      (check-proc 'protect remove-objs-hit-paddle 1 '3rd "1 argument")
      (check-proc 'protect remove-outside-objs    1 '4th "1 argument")
      (check-proc 'protect objs-posn              1 '5th "1 argument")
      ; --- 
      (ready-to-go?)
      (let* ((objs#  (length objs))
	     (PAD_Y0 (- (quotient SOUTH 2) (quotient PAD_LENGTH 2)))
	     (west   (box (make-posn 0 0)));; fake: to reuse move-paddle and make-landed
	     (east   (box (make-posn (- EAST (+ PAD_WIDTH PAD_DIST_WALL)) PAD_Y0))))
	(set! %landed-on-paddle? (make-landed-on-paddle east west))
	(draw-paddle (unbox east))
	;; --- the loop
	(let PLAY ((ball0 objs) (p0 (objs-posn objs)) (hits 0))
	  (if (null? ball0)
	      (cond
		[(= hits 0) (printf "You didn't catch any balls.")]
		[(= hits 1) (printf "You caught one ball. All others hit the wall.")]
		[else 
		 (printf "You caught ~s balls, ~s hit the wall." hits (- objs# hits))])
	      (begin
		(for-each draw-ball p0)
		(sleep SLEEP)
		(for-each clear-ball p0)
		(let* ((ball1 (move-objs ball0))
		       (ball2 (remove-objs-hit-paddle ball1))
		       (ball3 (remove-outside-objs ball2))
		       (p1    (objs-posn ball3)))
		  (move-paddle east west (ready-mouse-click (get-@VP)))
		  (PLAY ball3 p1 (+ hits (- (length ball1) (length ball2))))))))
	;; --- clean up
	(stop)))

    ;; Hidden Definitions
    ;; ==================

    ;; Hidden Functions: Tracing and Playing
    ;; -------------------------------------

    ;; TT : X (X -> posn) (X -> X) num[n] -> #t
    ;; effect: trace X's movement for n steps on canvas
    (define (TT ball ball-posn f e)
      (start2 EAST SOUTH)
      (let dl ((ball0 ball) (s 1))
	(if (> s e) #t
	  (let ((ball1 (f ball0 1)))
	    (and
	      (draw-solid-disk (ball-posn ball1) 3 'red)
	      (draw-solid-line (ball-posn ball0) (ball-posn ball1))
	      (dl ball1 (+ s 1)))))))

;    (define (check s make-ball ball-posn move)
;      (check-proc s make-ball  2 '1st "2 arguments")
;      (check-proc s ball-posn  1 '2nd "1 argument")
;      (check-proc s move       2 '3rd "2 arguments"))

    (define (play2 make-ball make-speed ball-posn move)
      (ready-to-go?)
      (let* ((rn-10-10 (lambda ()
			 (let ((n (random 20)))
			   (if (< n 10) (- n 10) (- n  9)))))
	     (posn0 (make-posn (quotient EAST 2) (quotient SOUTH 2)))
	     (PAD_Y0 (- (quotient SOUTH 2) (quotient PAD_LENGTH 2)))
	     (west (box (make-posn PAD_DIST_WALL                        PAD_Y0)))
	     (east (box (make-posn (- EAST (+ PAD_WIDTH PAD_DIST_WALL)) PAD_Y0)))
	     (start-time (current-seconds)))
	(set! %landed-on-paddle? (make-landed-on-paddle east west))
	(draw-paddle (unbox west))
	(draw-paddle (unbox east))
	;; --- the loop
	(let play ((ball0 (make-ball posn0 (make-speed (rn-10-10) (rn-10-10)))) (p0 posn0) (i 1))
	  (unless (or (< (posn-x p0) 0) (< EAST (posn-x p0)))
	    (draw-ball p0) (sleep SLEEP) (clear-ball p0)
	    (let* ((ball1 (move ball0 1)) (p1 (ball-posn ball1)))
	      (draw-solid-line p0 p1 TRACE-COLOR)
	      (move-paddle east west (ready-mouse-click (get-@VP)))
	      (if (zero? (modulo i SWITCH))
		  (play (make-ball p1 (make-speed (rn-10-10) (rn-10-10))) p1 1)
		  (play ball1 p1 (add1 i))))))
	;; --- clean up
	(printf "You kept the ball in play for ~s seconds.\n" (- (current-seconds) start-time))
	(stop)))

    ;; move-paddle : (box posn) (box posn) (union #f mouse-click) -> void
    ;; effect: modify the appropriate box, if mouse was clicked
    (define (move-paddle east west mc)
      (let ((new-posn (and mc (center-paddle (mouse-click-posn mc)))))
	(cond
	  ((not new-posn) (void))
	  ((in-west-zone? new-posn) (move-one-paddle west new-posn))
	  ((in-east-zone? new-posn) (move-one-paddle east new-posn)))))

    ;; move-one-paddle : (box posn) -> void
    ;; effect: modify the boxe, re-draw at appropriate place
    (define (move-one-paddle a-paddle a-posn)
      (clear-paddle (unbox a-paddle))
      (set-box! a-paddle (new-paddle (unbox a-paddle) (posn-y a-posn)))
      (draw-paddle (unbox a-paddle)))

    ;; ready-to-go? : -> void
    ;; effect: set up window, make announcement, wait for user to start the game
    (define (ready-to-go?)
      (start2 EAST SOUTH)
      (draw-solid-rect   (make-posn (- (quotient EAST 2) 100) 10) 200 20 BG-COLOR)
      ((draw-string (get-@VP)) (make-posn (- (quotient EAST 2)  65) 20)
       "Click anywhere when ready!")
      (let loop () (unless (ready-mouse-click (get-@VP)) (loop)))
      (draw-solid-rect   (make-posn (- (quotient EAST 2) 100) 10) 200 20 BG-COLOR))
 
    (define (start2 x y)
      (start x y)
      (draw-solid-rect (make-posn 0 0) EAST SOUTH BG-COLOR))

    ;; The Graphical Ball Representation
    ;; ---------------------------------
    (define BALL-RADIUS 5)
    (define (draw-ball p) (draw-solid-disk p BALL-RADIUS 'red))
    (define (clear-ball p) (draw-solid-disk p BALL-RADIUS 'white))

    ;(define (draw-ball p)
    ;  (set! draw-ball ((draw-pixmap-posn "Gifs/redball.gif") (get-@VP)))
    ;  (draw-ball p))
    ;(define (clear-ball p)
    ;  (set! clear-ball ((clear-pixmap-posn "Gifs/redball.gif") (get-@VP)))
    ;  (clear-ball p))


    ;; Global Properties (initialized by set-up!)
    ;; ------------------------------------------

    (define BG-COLOR   'green)
    (define BALL-COLOR 'red)
    (define PAD-COLOR  'blue)
    (define (set-pad)   (set! PAD-COLOR 'blue)) 
    (define (unset-pad) (set! PAD-COLOR 'white)) 
    (define TRACE-COLOR 'white)
    (define (set-trace) (set! TRACE-COLOR 'green)) 
    (define (unset-trace) (set! TRACE-COLOR 'white)) 

    (define SLEEP .15)
    (define SWITCH 10000)
    (define CENTER (quotient EAST 2))
    (define PADDLE-X EAST)
    (define PADDLE-Y (quotient SOUTH 2))

    ;; The Graphical Paddle Representation
    ;; ------------------------------------
    (define PAD_WIDTH  3)
    (define PAD_LENGTH 50)
    (define PAD_DIST_WALL  0)

    (define (draw-paddle paddle) 
      (draw-solid-rect paddle PAD_WIDTH PAD_LENGTH PAD-COLOR))
    (define (clear-paddle paddle)
      (draw-solid-rect paddle PAD_WIDTH PAD_LENGTH BG-COLOR))

    ;; center-paddle : posn -> posn
    (define (center-paddle p)
      (make-posn (posn-x p) (- (posn-y p) (quotient PAD_LENGTH 2))))
	
    (define (new-paddle paddle new-y)
      (make-posn (posn-x paddle) new-y))

    (define (in-west-zone? p) (<= 0 (posn-x p) CENTER))
    (define (in-east-zone? p) (<= CENTER (posn-x p) EAST))

    ;; make-landed-on-paddle :
    ;;   (box posn) (box posn) -> ( ball -> boolean )
    ;; to set up landed on paddle 
    (define (make-landed-on-paddle east west)
      (lambda (pball)
	(let ((y-pad (posn-y
		       (unbox
			 (cond
			   ((= EAST (posn-x pball)) east)
			   ((= WEST (posn-x pball)) west)
			   (else (error 'landed-on-paddle
				   "this ball has not reached a wall: ~s" pball)))))))
	  (<= y-pad (posn-y pball) (+ y-pad PAD_LENGTH)))))

    ;; landed-on-paddle : the paddles are initially in the middle of the
    ;; two walls
    (set! %landed-on-paddle?
      (make-landed-on-paddle
	(box (make-posn
	       (- PAD_DIST_WALL WEST)
	       (- (quotient SOUTH 2) (quotient PAD_LENGTH 2))))
	(box (make-posn
	       (- EAST (+ PAD_WIDTH PAD_DIST_WALL))
	       (- (quotient SOUTH 2) (quotient PAD_LENGTH 2))))))

    )))
