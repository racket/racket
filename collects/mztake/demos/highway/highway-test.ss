;; The program being debugged (a module in "highway.ss") generates fake speed readings over and over.

(require (lib "animation.ss" "frtime")) ;; needed for display-shapes


(mztake-process p ("highway.ss" [values-of-speed 3 4 bind 'speed]))
#| * Create a process to debug highway.ss

   * Add a tracepoint at line 3, column 4; in the program,
     this is right before the program sleeps for 1 second.

   * At this tracepoint, define "values-of-speed" to a FrTime eventstream that
     recieves events containing a single numerical value:
       - The (lexically-scoped) current value of the variables `speed' are send
         every time the code at line 3, column 4, is reached.
|#

(printf-b "runtime elapsed: ~a" (process:runtime/seconds p))
;; Prints how long the program has been running, in seconds

(printf-b "last ten speeds: ~a" (history-b 10 values-of-speed))
;; prints a FIFO list of the last 10 speeds seen

(map (lambda (an-x) (if (< an-x 55) 'ok 'too-fast!!))
     (history-b 10 values-of-speed))
;; prints a 

;; produces a list of shapes to draw/animate, taking in a number for speed
(define (make-speed-gauge speed)
  (let ([center (make-posn 200 200)])
    (list (make-circle center 170 "black")
          (make-circle center 160 "white")
          (make-rect (make-posn 0 202) 1000 1000 "white")
          (make-line (make-posn 30 201) (make-posn 370 201) "black")
          ;; draws the the half-circle guage
          
          ;; draws the red line for the current speed
          (make-line center
                     (posn+ center (make-posn (- (* 150 (cos (/ speed 30))))
                                              (- (* 150 (sin (/ speed 30))))))
                     "red"))))

(display-shapes (make-speed-gauge (hold values-of-speed)))
#| display-shapes takes a list of objects to draw.
   (hold values-of-speed) keeps track of the current value of speed,
   as seen on the eventstream, and that is passed to make-speed-guage,
   which gets called every time values-of-speed gets a new speed.
|#

(start/resume p)
;; Start the process for highway.ss