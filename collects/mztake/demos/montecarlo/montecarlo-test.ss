#|   The program being debugged (a module in the file "montecarlo.ss") runs an infinite loop,
     binding "x" and "y" to a random number between [-199,199] each iteration, and "pi"
     to the value of pi as calculated using this algorithm.
     
     This MzTake script draws only the points that are deemed *not* within
     a radius of 200 pixels from the center of the window.
|#


(require (lib "graphics.ss" "graphics"))
;; Needed for open-graphics, open-viewport, and draw-solid-ellipse

(open-graphics)
(define window (open-viewport "Debugger" 400 400))
#| This file doesn't animate a list of objects since the number of
   objects quickly reaches the thousands (slowing drawing time severly),
   and the dots are stationary -- so we just keep drawing the circles at
   the random coordinates that we get from the target program.

   See the doc for more information on this kind of drawing.
|#

(mztake-process p ("montecarlo.ss" [x/y/pi-trace 13 18 bind '(x y pi)]))
#| * Create a process to debug montecarlo.ss

   * Add a tracepoint at line 13, column 18; in the program,
     this is right after the cond determined that the point is not in
     the radius of the circle. [else ->(loop ...)]

   * At this tracepoint, define "x/y/pi-trace" to a FrTime eventstream that
     recieves events containing a list of the latest values of "x" "y" and "pi"
     in a list, every time the code at line 13, column 18, is reached.
|#


(define x/y/pi (hold x/y/pi-trace))
#| The local, time-varying variable "x/y/pi" is now is a FrTime behavior that always
   holds the current (latest) list of values from x/y/pi-trace.
|#


(define x (+ 200 (first x/y/pi)))
(define y (+ 200 (second x/y/pi)))
(define current-pi (third x/y/pi))
#| The local, time-varying variables "x" "y" and "current-pi" are bound to
   their respective values in the list from x/y/pi.
|#


(printf-b "total points chosen: ~a" (count-b x))
(printf-b "current computed value of pi: ~a" current-pi)

; more negative the better ...down to -14
(printf-b "log error: ~a" (log (abs (- current-pi 3.1415926))))

(map-e (lambda (x/y) ((draw-solid-ellipse window) (make-posn (first x/y) (second x/y))
                                                  3 3 "blue"))
       (changes (list x y)))
#| Every time the list (x y) changes (x and y get a new value), take this latest list value ("==>")
   and pass it to a function which draws a circle at the x,y coordinates in the list.
|#


(start/resume p)
;; Start the process for montecarlo.ss