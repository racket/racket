#|   The program being debugged (a module in "sine.ss") runs an infinite loop,
     binding "x" to a moment in time [-200,200], and "sin-x" to the sin(x) each iteration.

     This MzTake script plots the value of x over time, in sync with the execution of "sine.ss".
|#

(require (lib "animation.ss" "frtime")) ;; needed for display-shapes


(define-mztake-process p ("sine.ss" [sin/x-trace 5 8 bind '(sin-x x)]))
#| * Create a process to debug sine.ss

   * Add a tracepoint at line 5, column 8; in the program,
     this is right after the let values are bound, ->(if (x ...)

   * At this tracepoint, define "sin/x-trace" to a FrTime eventstream that
     recieves events containing a list of two elements:
       - The (lexically-scoped) current values of the variables `sin-x' and `x' are
         sent as a list every time the code at line 5, column 8, is reached.
|#

(printf-b "runtime elapsed: ~a" (process:runtime/seconds p))
;; Prints how long the program has been running, in seconds


(printf-b "sine.ss exited? ~a" (process:exited? p))
;; Prints out a behavior that tells you whether the mztake-process is still running...


(define sin/x (hold sin/x-trace))
;; the local variable "sin/x" now is a FrTime behavior that holds the current value of the list (sin-x x)


(define sin-x (+ 200 (first sin/x)))
(define x     (+ 200 (second sin/x)))
;; "x" and "sin-x" are the current values of (x + 200) and (sin(x) + 200)


(printf-b "x: ~a" x)
(printf-b "sin(x): ~a" sin-x)
;; Print the current values of x and sin-x


(display-shapes
 (list* (make-line (make-posn 0 200) (make-posn 400 200) "gray")
        (make-line (make-posn 200 0) (make-posn 200 400) "gray")
        ;;      draw horizontal and vertical gray lines
        
        (history-b 50 (changes (make-circle
                                (make-posn x sin-x)
                                5 ; diameter
                                (if (< 200 sin-x)
                                    (if (< 200 x) "blue" "darkblue")     ; Quadrants 3 and 4
                                    (if (< 200 x) "red" "darkred"))))))) ;           1 and 2
#|         * Make a circle at position (x, sin-x) with diameter 5 pixels, and a color based on the coords.
           * Everytime this value (the circle) changes (when the values of x and sin-x change)
             * Keep a history (as a FIFO list) of (up to) the last 50 circles that were created.
             * Pass this list to the display-shapes function, which will redraw everytime this list changes. 
|#

(start/resume p)
;; Start the process for sine.ss