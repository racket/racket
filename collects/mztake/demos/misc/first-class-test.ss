#| This program demonstrates how you can add traces to first class, anonymous functions,
   such as those passed to map, and the traces will still respond from anywhere
   the code is executed.
 
   This test also shows how you can bind to the same variable at different locations,
   and recieve different values, watching how an algorithm unfolds.

   Be sure you look at first-class.ss to see where the bindings are taken from, to get
   and idea of why they recieve different values from the same "x". |#

(define-mztake-process p ("first-class.ss" [x-before-let 3 29 bind 'x]
                                    [x-in-let     4 25 bind 'x]
                                    [x-after-let  5 11 bind 'x]))

(printf-b "Number of times x updates, should be 12: ~a"
          (count-e (merge-e x-before-let
                            x-in-let
                            x-after-let)))
#| merge-e takes multiple event streams and turns them into one event stream.
   count-e then counts how many pings are recieved on all three streams,
   in other words, how many times "x" updates in all the traces. |#

(printf-b "x before let, should be (2 4 6 7): ~a"  (history-b 4 x-before-let))
(printf-b "x in let, should be (6 10 14 16): ~a"   (history-b 4 x-in-let))
(printf-b "x after let, should be (5 9 13 15): ~a" (history-b 4 x-after-let))
#| Prints out a FIFO list containing the last 4 values seen by each trace. |#

(start/resume p)