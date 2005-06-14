(define x-before-let (trace (loc "first-class.ss" 3 29) (bind (x) x)))
(define x-in-let (trace (loc "first-class.ss" 4 25) (bind (x) x)))
(define x-after-let (trace (loc "first-class.ss" 5 11) (bind (x) x)))


(printf-b "Number of times x updates, should be 12: ~a"
          (count-b (merge-e x-before-let
                            x-in-let
                            x-after-let)))

(printf-b "x before let, should be (2 4 6 7): ~a"  (history-b 4 x-before-let))
(printf-b "x in let, should be (6 10 14 16): ~a"   (history-b 4 x-in-let))
(printf-b "x after let, should be (5 9 13 15): ~a" (history-b 4 x-after-let))

(set-running! true)