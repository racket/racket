#|   The program being debugged (a module in the file "random.ss") runs an infinite loop,
     binding "x" to a random number between [0,100) each iteration.
     
     This MzTake script draws a histogram of the values of x seen over time,
     in sync with the execution of "random.ss". This will run until one
     bar reaches the top of the screen.     

     This histogram provides three pieces of information:
      * Each bar represents a bin, the height represents how many times
        that "random" number was generated.

      * The brighter the blue, the faster that bin is growing compared
        to the others. The darker, the slower.

      * You can see a history of speeds over time based on how the colors
        change in each bin.

     Try looking for small groupings of bins where all are light, or all
     are dark -- these represent small trends in the numbers.

     Look for tortoises that stay low and black, and hares which are very
     active and bright.

     The bars drag a bit when moving upwards (the height goes up by 2, but
     the redrawing of the latest color goes down 10 pixels) so that you can
     spot vertical trends more easily.  |#


(require (lib "graphics.ss" "graphics")
         #| Needed for open-graphics, open-viewport, and draw-solid-ellipse |#
         
         (lifted mzscheme
                 make-hash-table
                 hash-table-put!
                 hash-table-get))
#|       "Lifted" is explained in FrTime's own documentation (plt/collects/frtime/doc.txt)
          Quickly put, lifting extends the functions listed above so they can take FrTime time-varying
          values (such as MzTake traces) as arguments. |#



(open-graphics)
(define window (open-viewport "Debugger" 600 500))
((draw-viewport window) (make-rgb 0.95 0.95 0.95))
#| This file doesn't animate a list of objects since the number of
   objects quickly reaches the thousands (slowing drawing time severly),
   and they are stationary -- so we just keep drawing the circles at
   their new heights based on the value in the hashtable.

   See the doc for more information on this kind of drawing. |#



(define-mztake-process p ("random.ss" [x-trace 4 6 bind 'x]))
#| * Create a process to debug random.ss

   * Add a tracepoint at line 4, column 6; in the program,
     this is right before the next iteration of the loop is called,
     ->(loop (random 200))

   * At this tracepoint, define "x-trace" to a FrTime eventstream that
     recieves events containing the latest value of "x" seen,
     every time the code at line 4, column 6, is reached. |#

(define largest-bin 0)
(define valcount (make-hash-table))
#| this will hold the counts for the histogram
   x is the key, and the number of times x shows up is the value |#


(hold (x-trace . -=> .(printf-b "largest count: ~a" largest-bin)))
#| Prints out the largest count every time we get a new x-trace event |#


(map-e (lambda (x)
         (let* ([new-cnt (add1 (hash-table-get valcount x (lambda () 0)))]
                [color (/ new-cnt (add1 largest-bin))])
           
           (when (= largest-bin 250)
             (kill p))
           ; when one of the bars reaches the top of the screen, kill the program.
           
           (when (> new-cnt largest-bin) (set! largest-bin new-cnt))
           ; keep track of the largest count
           
           (hash-table-put! valcount x new-cnt)
           ;; increment the value in the hashtable, starting from 0 if none exists.
           
           ((draw-solid-rectangle window) (make-posn (* x 6)  (- 500 (* 2 new-cnt)))
                                          6 10 ;; width height
                                          (make-rgb 0 (* 0.75 color) color))))
       x-trace)
#| Every time x-trace gets a new value, take this latest value and pass it to a function
   which increments the count in the hashtable, and draws a circle in the window at
   (* x 6) pixels from the left, and the height is (2 * the latest count in the hashtable for that x),
   making a color (MAKE-RGB) that is lighter based on how fast it is growing.
|#


(printf-b "count: ~a" (count-b x-trace))
#| prints the count of how many events x-trace got,
   aka how many values are in the histogram and on the screen.
|#


(start/resume p)
;; Start the process for random.ss