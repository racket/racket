;tests higher order annotation and redefinition of bindings

(debug-process p ("higher-order.ss" [x-before-let 3 29 bind 'x]
                                    [x-in-let     4 25 bind 'x]
                                    [x-after-let  5 11 bind 'x]))

(printf-b "Number of times x updates, should be 12: ~a"
          (count-e (merge-e x-before-let
                            x-in-let
                            x-after-let)))

(printf-b "x before let, should be (2 4 6 7): ~a"  (history-b 10 x-before-let))
(printf-b "x in let, should be (6 10 14 16): ~a"   (history-b 10 x-in-let))
(printf-b "x after let, should be (5 9 13 15): ~a" (history-b 10 x-after-let))

(start/resume p)