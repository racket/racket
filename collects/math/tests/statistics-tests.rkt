#lang typed/racket

(require math/statistics
         typed/rackunit)

(check-equal?
 (bin-samples '(5) <= '())
 empty)

(check-equal?
 (bin-samples '(5) <= '(4))
 (list (sample-bin 4 5 '(4) #f)))

(check-equal?
 (bin-samples '(5) <= '(5))
 (list (sample-bin 5 5 '(5) #f)))

(check-equal?
 (bin-samples '(5) <= '(6))
 (list (sample-bin 5 6 '(6) #f)))

(check-equal?
 (bin-samples '(5) <= '(4 5))
 (list (sample-bin 4 5 '(4 5) #f)))

(check-equal?
 (bin-samples '(5) <= '(5 6))
 (list (sample-bin 5 5 '(5) #f)
       (sample-bin 5 6 '(6) #f)))

(check-equal?
 (bin-samples '(5) <= '(4 6))
 (list (sample-bin 4 5 '(4) #f)
       (sample-bin 5 6 '(6) #f)))

(check-equal?
 (bin-samples '(5) <= '(4 5 6))
 (list (sample-bin 4 5 '(4 5) #f)
       (sample-bin 5 6 '(6) #f)))

(check-equal?
 (bin-samples '(4 8) <= '())
 (list (sample-bin 4 8 '() #f)))

(check-equal?
 (bin-samples '(4 8) <= '(2))
 (list (sample-bin 2 4 '(2) #f)
       (sample-bin 4 8 '() #f)))

(check-equal?
 (bin-samples '(4 8) <= '(4))
 (list (sample-bin 4 4 '(4) #f)
       (sample-bin 4 8 '() #f)))

(check-equal?
 (bin-samples '(4 8) <= '(6))
 (list (sample-bin 4 8 '(6) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(8))
 (list (sample-bin 4 8 '(8) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(10))
 (list (sample-bin 4 8 '() #f)
       (sample-bin 8 10 '(10) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(4 8))
 (list (sample-bin 4 4 '(4) #f)
       (sample-bin 4 8 '(8) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(4 10))
 (list (sample-bin 4 4 '(4) #f)
       (sample-bin 4 8 '() #f)
       (sample-bin 8 10 '(10) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(8 10))
 (list (sample-bin 4 8 '(8) #f)
       (sample-bin 8 10 '(10) #f)))

(check-equal?
 (bin-samples '(4 8) <= '(4 8 10))
 (list (sample-bin 4 4 '(4) #f)
       (sample-bin 4 8 '(8) #f)
       (sample-bin 8 10 '(10) #f)))

(check-equal?
 (bin-samples '(3 8) <= '(1 1 2 2 2 3 4 5 5 5 5 6 7 8 9 9))
 (list (sample-bin 1 3 '(1 1 2 2 2 3) #f)
       (sample-bin 3 8 '(4 5 5 5 5 6 7 8) #f)
       (sample-bin 8 9 '(9 9) #f)))
