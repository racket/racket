;;; test-set.scm  --  Jens Axel Søgaard  --  dec 10th 2005

; This file is included in "test-red-black-tree-set.scm" and
; in "test-list-set.scm", where the actual set-implementation
; is required.

(require "srfi-check.scm"
         (lib "42.ss" "srfi")
         (only (lib "list.ss") mergesort)
         (lib "67.ss" "srfi"))

;;; HELPERS

(define (same? l1 l2)
  (equal? (mergesort l1 <)
          (mergesort l2 <)))

(define (compare-mod2 x1 x2)
  (number-compare (modulo x1 2) (modulo x2 2)))

;;; TESTS  

; delete
(check (elements (delete 1 (set 2 1 3)))                                  (=> same?) (list 2 3))
(check (elements (delete 1 (insert* (list 3 4 5) (empty compare-mod2))))  (=> same?) (list 4))

; delete*
(check (elements (delete* (list 1 2) (set 4 2 1 3)))                                (=> same?) (list 3 4))
(check (elements (delete* (list 1 2) (insert* (list 3 4 5) (empty compare-mod2))))  (=> same?) (list))

; delete-all
(check (elements (delete-all 1 (set 2 1 3)))                                  (=> same?) (list 2 3))
(check (elements (delete-all 1 (insert* (list 3 4 5) (empty compare-mod2))))  (=> same?) (list 4))

; delete-min
(check (elements (delete-min (set 4 2 1 3)))  (=> same?) (list 2 3 4))
(check (elements (delete-min (set 1)))        (=> same?) (list))

; difference
(check (elements (difference (set 1 2 3) (set 3 4)))  (=> same?)  (list 1 2))
(check (elements (difference (set 1 2 3) (empty)))    (=> same?)  (list 1 2 3))
(check (elements (difference (empty) (set 1 2 3)))    (=> same?)  '())

; empty
(check (elements (empty))  (=> same?)  '())
(check (size (empty))  =>  0)

; empty?
(check (empty? (empty)) => #t)
(check (empty? (insert 1 (empty))) => #f)

; equal=?
(check (equal=? (set 1 3 5 7 9 2 4 6 8) (set 1 2 3 4 5 6 7 8 9))  =>  #t)
(check (equal=? (set 1 3 5 7 9 2 4 6 8) (set 1 2 3 4 5 6 7 8))    =>  #f)
(check (equal=? (empty) (empty)) => #t)
(check (equal=? (empty) (set 1)) => #f)
(check (equal=? (set 1) (empty)) => #f)

; find-min
(check (find-min (set 3 1 4)) => 1)
(check (find-min (set 1 )) => 1)
(check (find-min (set 3 7 9 2 3 4 1 4)) => 1)

; fold
(check (fold + 0 (insert* (list 1 2 3) (empty))) => 6)
(check (fold cons '() (insert* (list 1 2 3) (empty)))
       (=> same?)  (list 1 2 3))

; get
(check (get 1 (set 3 4 1)) => 1)
(check (get 1 (insert* (list 2 3 4 6) (empty compare-mod2)))
       => 3)

; insert
(check (elements (insert 1 (empty)))                         =>         (list 1))
(check (elements (insert 2 (empty)))                         =>         (list 2))
(check (elements (insert 1 (insert 2 (empty))))             (=> same?)  (list 1 2))
(check (elements (insert 1 (insert 2 (insert 3 (empty)))))  (=> same?)  (list 1 2 3))

; insert/combiner
(check (elements (insert/combiner 1 (empty) error))                                   =>         (list 1))
(check (elements (insert/combiner 1 (insert 2 (empty)) error))                       (=> same?)  (list 1 2))
(check (elements (insert/combiner 1 (insert 2 (insert 3 (empty compare-mod2))) max)) (=> same?)  (list 2 3))
(check (elements (insert/combiner 1 (insert 2 (insert 3 (empty compare-mod2))) min)) (=> same?)  (list 1 2))

; insert*
(check (elements (insert* '() (empty)))           =>         (list))
(check (elements (insert* (list 1) (empty)))      =>         (list 1))
(check (elements (insert* (list 1 2) (empty)))   (=> same?)  (list 2 1))
(check (elements (insert* (list 1 2 3) (empty))) (=> same?)  (list 3 2 1))

; insert*
(check (elements (insert*/combiner (list 1 2 3) (empty compare-mod2) max)) (=> same?)  (list 3 2))

; intersection
(check (elements (intersection (set 1 2 3) (set 3 4)))    (=> same?)  '(3))
(check (elements (intersection (set 1 2 3) (empty)))      (=> same?)  '())
(check (elements (intersection (empty) (set 1 2 3)))      (=> same?)  '())
(check (elements (intersection (set 1 2 3) (set 1 3 4)))  (=> same?)  '(1 3))

; intersection/combiner
(check (elements (intersection/combiner (set 1 2 3) (set 3 4) max))    (=> same?)  '(3))
(check (elements (intersection/combiner (set 1 2 3) (empty) max))      (=> same?)  '())
(check (elements (intersection/combiner (empty) (set 1 2 3) max))      (=> same?)  '())
(check (elements (intersection/combiner (set 1 2 3) (set 1 3 4) max))  (=> same?)  '(1 3))
(check (elements (intersection/combiner (list->set compare-mod2 (list 1 2))
                                        (list->set compare-mod2 (list 2 3))
                                        max)) (=> same?)  '(2 3))

; list->set
(check (elements (list->set (list 1 2 2 3)))  (=> same?) (list 1 2 3))
(check (elements (list->set (list)))          (=> same?) (list))

; list->set/combiner
(check (elements (list->set/combiner compare-mod2 (list 1 2 2 3) max))  (=> same?) (list 2 3))
(check (elements (list->set/combiner compare-mod2 (list) max))          (=> same?) (list))

; member?
(check (member? 1 (set 1 2 3)) => #t)
(check (member? 2 (set 1 2 3)) => #t)
(check (member? 3 (set 1 2 3)) => #t)
(check (member? 4 (set 1 2 3)) => #f)
(check (member? 1 (empty))     => #f)

; set
(check (elements (set)) => '())
(check (elements (set 1)) => (list 1))
(check (elements (set 1 1 1)) => (list 1))
(check (elements (set 1 2 1 3 1 4)) (=> same?) (list 1 2 3 4))

; set?
(check (set? (empty)) => #t)
(check (set? (insert 1 (empty))) => #t)
(check (set? '()) => #f)
(check (set? '(1)) => #f)

; singleton
(check (elements (singleton 1)) => (list 1))
(check (size (singleton '())) => 1)

; size
(check (size (empty)) => 0)
(check (size (insert* (list 1) (empty))) => 1)
(check (size (insert* (list 1 2) (empty))) => 2)
(check (size (insert* (list 1 2 3) (empty))) => 3)

; subset?
(check (subset? (set 1 2 3) (set 1 2 3 4)) => #t)
(check (subset? (set 1 2 3) (set 1 2 3))   => #t)
(check (subset? (set 1 2 3) (set 1 2))     => #f)
(check (subset? (set 1 2 3) (empty))       => #f)
(check (subset? (empty)     (set 1 2 3))   => #t)
(check (subset? (empty)     (empty))       => #t)

; union
(check (elements (union (set 1 2 3) (set 1 2 3 4))) (=> same?)  (list 1 2 3 4))
(check (elements (union (set 1 2 3) (set 1 2 3)))   (=> same?)  (list 1 2 3))
(check (elements (union (set 1 2 3) (set 1 2)))     (=> same?)  (list 1 2 3))
(check (elements (union (set 1 2 3) (empty)))       (=> same?)  (list 1 2 3))
(check (elements (union (empty)     (set 1 2 3)))   (=> same?)  (list 1 2 3))
(check (elements (union (empty)     (empty)))       (=> same?)  (list))

; select
(check (select (set 1))
       => 1)

; set-ec
(check (elements (set-ec default-compare (: i 3) i))
       (=> same?) (list 2 1 0))

; :set and : 
(check (list-ec (:set x (insert* (list 1 2 3) (empty))) x)
       (=> same?) (list 3 2 1))
(check (list-ec (: x (insert* (list 1 2 3) (empty))) x)
       (=> same?) (list 3 2 1))


(check-report)
