;;; test-heap.scm  --  Jens Axel Søgaard  --  dec 12th 2005

(require "srfi-check.scm"
         (lib "42.ss" "srfi")
         (lib "67.ss" "srfi")
         (only (lib "list.ss") mergesort))

;;; HELPERS

(define (same? l1 l2)
  (equal? (mergesort l1 <)
          (mergesort l2 <)))

(define (compare-mod2 x1 x2)
  (number-compare (modulo x1 2) (modulo x2 2)))

;;; TESTS

; count
(check (count 1 (heap 1 2 3 4 1 2 3)) => 2)
(check (count 4 (heap 1 2 3 4 1 2 3)) => 1)
(check (count 7 (heap 1 2 3 4 1 2 3)) => 0)

; delete
(check (elements (delete 1 (heap)))                (=> same?) (list))
(check (elements (delete 1 (heap 1)))              (=> same?) (list))
(check (elements (delete 1 (heap 1 2)))            (=> same?) (list 2))
(check (elements (delete 1 (heap 1 2 3)))          (=> same?) (list 2 3))
(check (elements (delete 1 (heap 3 2 1 4 1 2 5)))  (=> same?) (list 3 2 1 4 2 5))
(check (elements (delete 1 (heap)))                (=> same?) (list))
(check (elements (delete 6 (heap 2 1 3)))          (=> same?) (list 1 2 3))
(check (elements (delete 7 (heap 2 3)))            (=> same?) (list 2 3))

; delete-all
(check (elements (delete-all 1 (heap)))                      (=> same?) (list))
(check (elements (delete-all 1 (heap 1)))                    (=> same?) (list))
(check (elements (delete-all 1 (heap 1 2)))                  (=> same?) (list 2))
(check (elements (delete-all 1 (heap 1 2 3)))                (=> same?) (list 2 3))
(check (elements (delete-all 1 (heap 3 2 1 4 1 2 5)))        (=> same?) (list 3 2 4 2 5))
(check (elements (delete-all 5 (heap 3 5 2 1 5 4 1 5 2 5)))  (=> same?) (list 3 2 1 4 1 2))
(check (elements (delete-all 1 (heap)))                      (=> same?) (list))
(check (elements (delete-all 6 (heap 2 1 3)))                (=> same?) (list 1 2 3))
(check (elements (delete-all 7 (heap 2 3)))                  (=> same?) (list 2 3))

; delete-min
(check (find-min (delete-min (insert* (list 5 4 1 3 2) (empty))))
       => 2)
(check (empty? (delete-min
                (delete-min
                 (delete-min
                  (delete-min
                   (delete-min (insert* (list 5 4 1 3 2) (empty))))))))
       => #t)

; empty?
(check (empty? (empty)) => #t)
(check (empty? (insert 1 (empty))) => #f)

; find-min
(check (find-min (insert* (list 1) (empty)))          => 1)
(check (find-min (insert* (list 2 1) (empty)))        => 1)
(check (find-min (insert* (list 2 1 3) (empty)))      => 1)
(check (find-min (insert* (list 5 4 1 3 2) (empty)))  => 1)
(check (empty? (delete-min
                (delete-min
                 (delete-min
                  (delete-min
                   (delete-min (insert* (list 5 4 1 3 2) (empty))))))))
       => #t)

; elements
;  - see insert*

; fold
(check (fold + 0 (insert* (list 1 2 3) (empty))) => 6)
(check (mergesort (fold cons '() (insert* (list 1 2 3) (empty))) <)
       => (list 1 2 3))

; get
(check (get 1 (heap 3 4 1)) => 1)
(check (get 7 (heap 3 4 1)) => #f)
(check (get 0 (heap-ec default-compare (: i -10 10) (* i i))) => 0)
(check (get 1 (insert* (list 2 3 4 6) (empty compare-mod2)))
       => 3)

; heap?
(check (heap? (empty)) => #t)
(check (heap? (insert 1 (empty))) => #t)
(check (heap? '()) => #f)
(check (heap? '(1)) => #f)

; heap-ec
(check (mergesort (elements (heap-ec default-compare (: i 3) i)) <)
       => (list 0 1 2))

; :heap and : 
(check (list-ec (:heap x (insert* (list 2 1 3) (empty))) x)
       => (list 1 2 3))
(check (list-ec (: x (insert* (list 1 3 2) (empty))) x)
       => (list 1 2 3))

; insert*
(check (elements (insert* '() (empty)))           =>         (list))
(check (elements (insert* (list 1) (empty)))      =>         (list 1))
(check (elements (insert* (list 1 2) (empty)))   (=> same?)  (list 2 1))
(check (elements (insert* (list 1 2 3) (empty))) (=> same?)  (list 3 2 1))

; list->heap
(check (find-min (list->heap (list 4 1 2 8 1 3))) => 1)
(check (find-min (list->heap default-compare (list 4 1 2 8 1 3))) => 1)

; select
(check (select (heap 1)) => 1)

; singleton
(check (size (singleton 3)) => 1)
(check (empty? (singleton 3)) => #f)
(check (find-min (singleton 3)) => 3)
(check (find-min (singleton default-compare 3)) => 3)

; size
(check (size (insert* (list 1 2 3) (empty))) => 3)
(check (size (insert* (list 1 2) (empty))) => 2)
(check (size (insert* (list 1) (empty))) => 1)
(check (size (empty)) => 0)

; union
(check (find-min (union (insert* (list 1 2 3) (empty))
                        (insert* (list 4 5)   (empty))))
       => 1)
(check (find-min (union (insert* (list 1 2 3) (empty))
                        (insert* (list 1 4 5) (empty))))
       => 1)
(check (find-min (union (insert* (list 2 3)    (empty))
                        (insert* (list 4 1 5)  (empty))))
       => 1)


;;; TEST REPORT

(check-report)
