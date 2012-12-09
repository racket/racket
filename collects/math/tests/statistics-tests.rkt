#lang typed/racket

(require math/statistics
         typed/rackunit)

(check-equal?
 (bin-samples '() '(5) <=)
 empty)

(check-equal?
 (bin-samples '(4) '(5) <=)
 (list (sample-bin '(4) #f 4 5)))

(check-equal?
 (bin-samples '(5) '(5) <=)
 (list (sample-bin '(5) #f 5 5)))

(check-equal?
 (bin-samples '(6) '(5) <=)
 (list (sample-bin '(6) #f 5 6)))

(check-equal?
 (bin-samples '(4 5) '(5) <=)
 (list (sample-bin '(4 5) #f 4 5)))

(check-equal?
 (bin-samples '(5 6) '(5) <=)
 (list (sample-bin '(5) #f 5 5)
       (sample-bin '(6) #f 5 6)))

(check-equal?
 (bin-samples '(4 6) '(5) <=)
 (list (sample-bin '(4) #f 4 5)
       (sample-bin '(6) #f 5 6)))

(check-equal?
 (bin-samples '(4 5 6) '(5) <=)
 (list (sample-bin '(4 5) #f 4 5)
       (sample-bin '(6) #f 5 6)))

(check-equal?
 (bin-samples '() '(4 8) <=)
 (list (sample-bin '() #f 4 8)))

(check-equal?
 (bin-samples '(2) '(4 8) <=)
 (list (sample-bin '(2) #f 2 4)
       (sample-bin '() #f 4 8)))

(check-equal?
 (bin-samples '(4) '(4 8) <=)
 (list (sample-bin '(4) #f 4 4)
       (sample-bin '() #f 4 8)))

(check-equal?
 (bin-samples '(6) '(4 8) <=)
 (list (sample-bin '(6) #f 4 8)))

(check-equal?
 (bin-samples '(8) '(4 8) <=)
 (list (sample-bin '(8) #f 4 8)))

(check-equal?
 (bin-samples '(10) '(4 8) <=)
 (list (sample-bin '() #f 4 8)
       (sample-bin '(10) #f 8 10)))

(check-equal?
 (bin-samples '(4 8) '(4 8) <=)
 (list (sample-bin '(4) #f 4 4)
       (sample-bin '(8) #f 4 8)))

(check-equal?
 (bin-samples '(4 10) '(4 8) <=)
 (list (sample-bin '(4) #f 4 4)
       (sample-bin '() #f 4 8)
       (sample-bin '(10) #f 8 10)))

(check-equal?
 (bin-samples '(8 10) '(4 8) <=)
 (list (sample-bin '(8) #f 4 8)
       (sample-bin '(10) #f 8 10)))

(check-equal?
 (bin-samples '(4 8 10) '(4 8) <=)
 (list (sample-bin '(4) #f 4 4)
       (sample-bin '(8) #f 4 8)
       (sample-bin '(10) #f 8 10)))

(check-equal?
 (bin-samples '(1 1 2 2 2 3 4 5 5 5 5 6 7 8 9 9) '(3 8) <=)
 (list (sample-bin '(1 1 2 2 2 3) #f 1 3)
       (sample-bin '(4 5 5 5 5 6 7 8) #f 3 8)
       (sample-bin '(9 9) #f 8 9)))
