(require (lib "graphics.ss" "graphics"))


(open-graphics)
(define window (open-viewport "Debugger" 400 400))


(define-mztake-process p ("montecarlo.ss" [x/y/pi-trace 13 13 bind '(x y pi)]))


(define x/y/pi (hold x/y/pi-trace))


(define x (+ 200 (first x/y/pi)))
(define y (+ 200 (second x/y/pi)))
(define current-pi (third x/y/pi))


(printf-b "total points chosen: ~a" (count-b (changes x)))
(printf-b "current computed value of pi: ~a" current-pi)
(printf-b "log error: ~a" (log (abs (- current-pi 3.141592653))))


((draw-viewport window) "wheat")
((draw-solid-ellipse window) (make-posn -4 -4) 408 408 "black")
((draw-solid-ellipse window) (make-posn 0 0) 400 400 "sienna")


(map-e (lambda (x/y) ((draw-solid-ellipse window) (make-posn (first x/y) (second x/y))
                                                  3 3 "black"))
       (changes (list x y)))

(start/resume p)
