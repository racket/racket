(require (lib "graphics.ss" "graphics")
	 (lib "match.ss"))


(open-graphics)
(define window (open-viewport "Debugger" 400 400))

(define/bind (loc "montecarlo.ss" 13 13) x y pi)


(printf-b "total points chosen: ~a" (count-b (changes x)))
(printf-b "current computed value of pi: ~a" current-pi)
(printf-b "log error: ~a" (log (abs (- current-pi 3.141592653))))


((draw-viewport window) "wheat")
((draw-solid-ellipse window) (make-posn -4 -4) 408 408 "black")
((draw-solid-ellipse window) (make-posn 0 0) 400 400 "sienna")


(map-e (match-lambda [(x y) ((draw-solid-ellipse window) (make-posn x y)
			     3 3 "black")])
       (changes (list x y)))

(set-running! true)
