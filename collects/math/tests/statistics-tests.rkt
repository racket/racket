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

(check-equal?
 (bin-samples '(3 8) <=
              '(1 1 2 2 2 3 4 5 5 5 5 6 7 8 9 9)
              '(1 1.1 2 2.1 2.2 3 4 5 5.1 5.2 5.3 6 7 8 9 9.1))
 (list (sample-bin 1 3 '(1 1 2 2 2 3) '(1 1.1 2 2.1 2.2 3))
       (sample-bin 3 8 '(4 5 5 5 5 6 7 8) '(4 5 5.1 5.2 5.3 6 7 8))
       (sample-bin 8 9 '(9 9) '(9 9.1))))

(check-equal?
 (bin-samples/key '(2 4) <= (inst car Real Symbol)
                  '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
 (list (sample-bin 1 2 '((1 . a) (2 . b)) #f)
       (sample-bin 2 4 '((3 . c) (4 . d)) #f)
       (sample-bin 4 5 '((5 . e)) #f)))

(for: ([p  '(0 1/6 2/6 3/6 4/6 5/6 6/6)])
  (check-equal? (quantile p < '(1 2 3) '(1 1 1))
                (quantile p < '(1 2 3))))

(for: ([p  '(0 1/8 2/8 3/8 4/8 5/8 6/8 7/8 8/8)])
  (check-equal? (quantile p < '(1 2 3 4) '(1 1 1 1))
                (quantile p < '(1 2 3 4))))
