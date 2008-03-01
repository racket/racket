;;; test-bag.scm  --  Jens Axel Søgaard  --  dec 20th 2005


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

; bag
(check (elements (bag)) => '())
(check (elements (bag 1)) => (list 1))
(check (elements (bag 1 1 1)) => (list 1 1 1))
(check (elements (bag 1 2 1 3 1 4)) (=> same?) (list 1 1 1 2 3 4))

; bag?
(check (bag? (empty)) => #t)
(check (bag? (insert 1 (empty))) => #t)
(check (bag? '()) => #f)
(check (bag? '(1)) => #f)

; bag-ec
(check (elements (bag-ec default-compare (: i 3) i))
       (=> same?) (list 2 1 0))

; :bag and : 
(check (list-ec (:bag x (insert* (list 1 2 3) (empty))) x)
       (=> same?) (list 3 2 1))
(check (list-ec (: x (insert* (list 1 2 3) (empty))) x)
       (=> same?) (list 3 2 1))

; count
(check (count 1 (bag 3 1 2 1 4)) => 2)
(check (count 1 (bag 3 4 5)) => 0)

; delete
(check (elements (delete 1 (bag 2 1 3)))                                  (=> same?) (list 2 3))
(check (elements (delete 1 (bag 2 1 1 3)))                                (=> same?) (list 1 2 3))
(check (elements (delete 1 (insert* (list 3 4) (empty compare-mod2))))    (=> same?) (list 4))

; delete-all
(check (elements (delete-all 1 (bag 2 1 1 3 1)))                               (=> same?) (list 2 3))
(check (elements (delete-all 1 (insert* (list 3 4 5) (empty compare-mod2))))   (=> same?) (list 4))

; delete*
(check (elements (delete* (list 1 2) (bag 4 2 1 3)))                              (=> same?) (list 3 4))
(check (elements (delete* (list 1 2) (insert* (list 3 4) (empty compare-mod2))))  (=> same?) (list))

; difference
(check (elements (difference (bag 1 2 3)          (bag 3 4)))     (=> same?)  (list 1 2))
(check (elements (difference (bag 1 2 3)          (empty)))       (=> same?)  (list 1 2 3))
(check (elements (difference (empty)              (bag 1 2 3)))   (=> same?)  '())
(check (elements (difference (bag 1 1 2 3 3 4 5)  (bag 3 4 5)))   (=> same?)  (list 1 1 2 3))
(check (elements (difference (bag 1 2 2 3)        (empty)))       (=> same?)  (list 1 2 2 3))
(check (elements (difference (empty)              (bag 1 2 2 3))) (=> same?)  '())

; empty
(check (elements (empty))  (=> same?)  '())
(check (size (empty))  =>  0)

; empty?
(check (empty? (empty)) => #t)
(check (empty? (insert 1 (empty))) => #f)

; equal=?
(check (equal=? (bag 1 3 5 7 9 2 4 6 8) (bag 1 2 3 4 5 6 7 8 9))  =>  #t)
(check (equal=? (bag 1 3 5 7 9 2 4 6 8) (bag 1 2 3 4 5 6 7 8))    =>  #f)
(check (equal=? (empty) (empty)) => #t)
(check (equal=? (empty) (bag 1)) => #f)
(check (equal=? (bag 1) (empty)) => #f)

; fold
(check (fold + 0 (insert* (list 1 2 3) (empty))) => 6)
(check (fold cons '() (insert* (list 1 2 3) (empty)))
       (=> same?)  (list 1 2 3))

; fold/no
(check (fold/no (lambda (x no sum) (+ (* x no) sum))
             0 (bag 1 1 1 2 5 5))
       => 15)

; get
(check (get 1 (bag 3 4 1)) => 1)
(check (get 1 (insert* (list 2 3 4 6) (empty compare-mod2))) => 3)

; insert, elements 
(check (elements (insert 1 (empty)))                         =>         (list 1))
(check (elements (insert 2 (empty)))                         =>         (list 2))
(check (elements (insert 1 (insert 2 (empty))))             (=> same?)  (list 1 2))
(check (elements (insert 1 (insert 2 (insert 3 (empty)))))  (=> same?)  (list 1 2 3))

; insert*
(check (elements (insert* '() (empty)))           =>         (list))
(check (elements (insert* (list 1) (empty)))      =>         (list 1))
(check (elements (insert* (list 1 2) (empty)))   (=> same?)  (list 2 1))
(check (elements (insert* (list 1 2 3) (empty))) (=> same?)  (list 3 2 1))

; intersection
(check (elements (intersection (bag 1 2 3)   (bag 3 4)))        (=> same?)  '(3))
(check (elements (intersection (bag 1 1 2 3) (bag 1 3 4)))      (=> same?)  '(1 3))
(check (elements (intersection (bag 1 1 2 3) (bag 1 1 3 4)))    (=> same?)  '(1 1 3))
(check (elements (intersection (bag 1 1 2 3) (bag 1 1 1 3 4)))  (=> same?)  '(1 1 3))
(check (elements (intersection (bag 1 2 3)   (empty)))          (=> same?)  '())
(check (elements (intersection (empty)       (bag 1 2 3)))      (=> same?)  '())
(check (elements (intersection (bag 1 2 3)   (bag 1 3 4)))      (=> same?)  '(1 3))

; list->bag
(check (elements (list->bag (list 1 2 3 3))) (=> same?) (list 1 2 3 3))
(check (elements (list->bag default-compare (list 1 2 3 3))) (=> same?) (list 1 2 3 3))

; member?
(check (member? 1 (bag 1 2 3)) => #t)
(check (member? 2 (bag 1 2 3)) => #t)
(check (member? 3 (bag 1 2 3)) => #t)
(check (member? 4 (bag 1 2 3)) => #f)
(check (member? 1 (empty))     => #f)

; singleton
(check (elements (singleton 1)) => (list 1))
(check (size (singleton '())) => 1)
(check (size (singleton default-compare 2)) => 1)
(check (select (singleton default-compare 2)) => 2)

; size
(check (size (empty)) => 0)
(check (size (insert* (list 1) (empty))) => 1)
(check (size (insert* (list 1 2) (empty))) => 2)
(check (size (insert* (list 1 2 3) (empty))) => 3)

; subbag?
(check (subbag? (bag 1 2 3)   (bag 1 2 3 4)) => #t)
(check (subbag? (bag 1 2 3)   (bag 1 2 3))   => #t)
(check (subbag? (bag 1 2 2 3) (bag 1 2 3))   => #f)
(check (subbag? (bag 1 2 3)   (bag 1 2 2 3)) => #t)
(check (subbag? (bag 1 2 3)   (bag 1 2))     => #f)
(check (subbag? (bag 1 2 3)   (empty))       => #f)
(check (subbag? (empty)       (bag 1 2 3))   => #t)
(check (subbag? (empty)       (empty))       => #t)

; union
(check (elements (union (bag 1 2 3) (bag 1 2 3 4))) (=> same?)  (list 1 1 2 2 3 3 4))
(check (elements (union (bag 1 2 3) (bag 1 2 3)))   (=> same?)  (list 1 1 2 2 3 3))
(check (elements (union (bag 1 2 3) (bag 1 2)))     (=> same?)  (list 1 1 2 2 3))
(check (elements (union (bag 1 2 3) (empty)))       (=> same?)  (list 1 2 3))
(check (elements (union (empty)     (bag 1 2 3)))   (=> same?)  (list 1 2 3))
(check (elements (union (empty)     (empty)))       (=> same?)  (list))

; select
(check (select (bag 1)) => 1)


;;; REPORT

(check-report)
