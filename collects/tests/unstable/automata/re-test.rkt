#lang racket
(require unstable/automata/machine
         unstable/automata/re
         unstable/automata/re-ext
         unstable/match
         tests/eli-tester)

(define-syntax-rule (test-re* R (succ ...) (fail ...))
  (let ()
    (define r (re R))
    (test #:failure-prefix (format "~s" 'R)
          (test
           (machine-accepts? r succ) ...
           (not (machine-accepts? r fail)) ...))))
(define-syntax-rule (test-re R (succ ...) (fail ...))
  (test (test-re* R (succ ...) (fail ...))
        (test-re* (complement R) (fail ...) (succ ...))))

(test
 (test-re epsilon
          [(list)]
          [(list 0)])
 
 (test-re nullset
          []
          [(list) (list 1)])
 
 (test-re "A"
          [(list "A")]
          [(list)
           (list "B")])
 
 (test-re (complement "A")
          [(list)
           (list "B")
           (list "A" "A")]
          [(list "A")])
 
 (test-re (union 0 1)
          [(list 1)
           (list 0)]
          [(list)
           (list 0 1)
           (list 0 1 1)])
 
 (test-re (seq 0 1)
          [(list 0 1)]
          [(list)
           (list 0)
           (list 0 1 1)])
 
 (test-re (star 0)
          [(list)
           (list 0)
           (list 0 0)]
          [(list 1)])
 
 (test-re (opt "A")
          [(list)
           (list "A")]
          [(list "B")])
 
 (test-re (plus "A")
          [(list "A")
           (list "A" "A")]
          [(list)])
 
 (test-re (rep "A" 3)
          [(list "A" "A" "A")]
          [(list)
           (list "A")
           (list "A" "A")])
 
 (test-re (difference (? even?) 2)
          [(list 4)
           (list 6)]
          [(list 3)
           (list 2)])
 
 (test-re (intersection (? even?) 2)
          [(list 2)]
          [(list 1)
           (list 4)])
 
 (test-re (complement (seq "A" (opt "B")))
          [(list "A" "B" "C")]
          [(list "A")
           (list "A" "B")])
 
 (test-re (seq epsilon 1)
          [(list 1)]
          [(list 0)
           (list)])
 
 (test-re (seq 1 epsilon)
          [(list 1)]
          [(list 0)
           (list)])
 
 (test-re (seq epsilon
               (union (seq (star 1) (star (seq 0 (star 1) 0 (star 1))))
                      (seq (star 0) (star (seq 1 (star 0) 1 (star 0)))))
               epsilon)
          [(list 1 0 1 0 1)
           (list 0 1 0 1 0)
           (list 1 0 1 1 0 1)
           (list 0 1 0 0 1 0)
           (list)]
          [(list 1 0)])
 
 (test-re (star (complement 1))
          [(list 0 2 3 4)
           (list)
           (list 2)
           ; This is correct, because the complement machine
           ; could accept '(234 5 9 1), which is not '(1)
           ; Then the star kicks in and it accepts '(9 0)
           (list 234 5 9 1 9 0)
           (list 1 0)
           (list 0 1)]
          [(list 1)])
 
 (test-re (dseq x (== x))
          [(list 0 0)
           (list 1 1)]
          [(list)
           (list 1)
           (list 1 0)])
 
 (test-re (seq 1 (seq 2 3))
          [(list 1 2 3)]
          [(list 1)
           (list 1 2)])
 
 (test-re (rec x
            (union #f
                   (seq 1 ,x)))
          [(list #f)
           (list 1 #f)
           (list 1 1 #f)
           (list 1 1 1 #f)]
          [(list 1)
           (list 2)
           (list 1 1 2)])
 
 (test-re (seq/close 1 2 3)
          [(list)
           (list 1)
           (list 1 2)
           (list 1 2 3)]
          [(list 2)
           (list 1 3)
           (list 2 3)
           (list 1 2 3 4)])
 )
