#|   The program being debugged (a module in "sine.ss") runs an infinite loop,
     binding "x" to a moment in time [-200,200], and "sin-x" to the sin(x/20) each iteration.

     This MzTake script plots the value of x over time, in sync with the execution of "sine.ss". |#

(require (lib "animation.ss" "frtime")) ;; needed for display-shapes


(define-mztake-process p ("sine.ss" [x/sinx-trace 5 8 bind '(x sin-x)]))
#| * Create a process to debug sine.ss

   * Add a tracepoint at line 5, column 8; in the program,
     this is right after the let values are bound, ->(if (x ...)

   * At this tracepoint, define "x/sinx-trace" to be a FrTime eventstream that
     recieves events containing a list of two elements -- the current values
     of the variables `x' and `sin-x', respectively. |#


(define x/sinx (hold x/sinx-trace))
#| the local variable "sin/x" now is a FrTime behavior that holds the current value of the list (sin-x x) |#

(define x     (first x/sinx))
(define sin-x (second x/sinx))
#| the local variables x, sin-x hold their current values |#


(printf-b "x: ~a" x)
(printf-b "sin(x/20): ~a" sin-x)
#| Print the current values of x and sin-x |#

(printf-b "largest  x: ~a  sin(x/20): ~a"
          (largest-val-b (changes (first x/sinx)))
          (largest-val-b (changes (second x/sinx))))

(printf-b "smallest x:~a  sin(x/20):~a"
          (smallest-val-b (changes (first x/sinx)))
          (smallest-val-b (changes (second x/sinx))))


(display-shapes
 (list* (make-line (make-posn 0 200) (make-posn 400 200) "gray")
        (make-line (make-posn 200 0) (make-posn 200 400) "gray")
        #| draw horizontal and vertical gray lines |#
        
        (let ([x (+ 200 x)]
              [sin-x (+ 200 (* 100 sin-x))])
          (history-b 50 (changes (make-circle
                                  (make-posn x sin-x)
                                  5
                                  (if (< 200 sin-x)
                                      (if (< 200 x) "blue" "darkblue")      #| Quadrants 3 and 4 |#
                                      (if (< 200 x) "red" "darkred")))))))) #|           1 and 2 |#

#|      Make a circle at position x:(x + 200) and y:(100*sin(x/20) + 200) (scaled so we can draw it on screen)
        with diameter of 5 pixels, and a color based on which quadrant the coordinate is in.
        
        Every time this value (the circle) changes (when the values of x and sin-x change):
          * Keep a history (as a FIFO list) of (up to) the last 50 circles that were created.
          * Pass this list to the display-shapes function, which will redraw every time this list changes. |#


(start/resume p) #| Start the process for sine.ss |#