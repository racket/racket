#|   The program being debugged (a module in the file "random-xs.ss") runs an infinite loop,
     binding "x" to a random number between [0,199] each iteration.
     
     This MzTake script draws a histogram of the values of x seen over time,
     in sync with the execution of "random-xs.ss".
|#


(require (lib "graphics.ss" "graphics")
         ;; Needed for open-graphics, open-viewport, and draw-solid-ellipse
         
         (lifted mzscheme
                 make-hash-table
                 hash-table-put!
                 hash-table-get))
#|       "Lifted" is explained in FrTime's own documentation (plt/collects/frtime/doc.txt)
          Quickly put, lifting extends the functions listed above so they can take FrTime time-varying
          values (such as MzTake traces) as arguments. 
|#


(open-graphics)
(define window (open-viewport "Debugger" 600 500))
#| This file doesn't animate a list of objects since the number of
   objects quickly reaches the thousands (slowing drawing time severly),
   and they are stationary -- so we just keep drawing the circles at
   their new heights based on the value in the hashtable.

   See the doc for more information on this kind of drawing.
|#

(define-mztake-process p ("random-Xs.ss" [x-trace 4 6 bind 'x]))
#| * Create a process to debug random-xs.ss

   * Add a tracepoint at line 4, column 6; in the program,
     this is right before the next iteration of the loop is called,
     ->(loop (random 200))

   * At this tracepoint, define "x-trace" to a FrTime eventstream that
     recieves events containing the latest value of "x" seen,
     every time the code at line 4, column 6, is reached.
|#

(define x (hold x-trace))
#| The local, time-varying variable "x" is now is a FrTime behavior that always
   holds the current (latest) value of x-trace.
|#

(define valcount (make-hash-table))
;; this will hold the counts for the histogram

(map-e (lambda (x)
         (hash-table-put! valcount x (add1 (hash-table-get valcount x (lambda () 0))))
         ;; increment the value in the hashtable, starting from 0 if none exists.
         ((draw-solid-ellipse window) (make-posn (* x 3)
                                                 (- 500 (* 3 (hash-table-get valcount x (lambda () 1)))))
                                                    4 4 "blue"))
       (changes x))
#| Every time the local variable x changes (x-trace gets a new value), take this latest value ("==>")
   and pass it to a function which increments the count in the hashtable, and draws a circle in the window
   at (* x 3) pixels from the left, and the height is (3 * the latest count in the hashtable for that x).
|#

(printf-b "x: ~a" x)
(printf-b "count: ~a" (count-e (changes x)))
#| prints the current value of x and a count of how many times x has changed,
   in other words, how many values are in the histogram
|#

(let ([cnt (count-e (changes x))])
  (when (= 2000 cnt) (pause p)))
#| This binds the same type of count seen above to cnt,
   when the histogram is showing 2000 values, pause the program
   the next time the breakpoint is reached before doing anything else.

   Then try restarting it with (start/resume p)
|#

(start/resume p)
;; Start the process for random-xs.ss