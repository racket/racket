(require (lib "graphics.ss" "graphics"))

(open-graphics)
(define window (open-viewport "Debugger" 400 400))

(debug-process p ("montecarlo.ss" [x/y/pi-trace 12 18 bind '(x y pi)]))

(define x/y/pi (hold x/y/pi-trace))

(define x (+ 200 (first x/y/pi)))
(define y (+ 200 (second x/y/pi)))
(define current-pi (third x/y/pi))

(printf-b "total: ~a" (count-b x))
(printf-b "current pi: ~a" current-pi)
; more negative the better ...down to -14
(printf-b "log error: ~a" (log (abs (- current-pi 3.1415926))))

((changes (list x y))
 . ==> .
 (lambda (x/y)
   ((draw-solid-ellipse window) (make-posn (first x/y) (second x/y))
                                3 3 "blue")))

(start/resume p)
