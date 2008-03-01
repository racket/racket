;;; test-queue.scm  --  Jens Axel Søgaard

; CHOOSE BETWEEN QUEUES AND DEQUES BELOW

; TODO: Split queue and deque test in seperate files.
; TODO: deque.scm is not entirely covered by the tests
; TODO: write tests for last

; The functions are ordered alphabetically.

; COMMENT THE RELEVANT LINE IN
;(require "../queue.scm") (define *deque* #f)
(require "../deque.scm") (define *deque* #t)
                      
(require "srfi-check.scm"
         (lib "42.ss" "srfi"))

(if *deque*
    (begin
      ; deqeue-ec
      (check (elements (deque-ec (: i 3) i))
             => (list 0 1 2))
      ; :deque
      (check (list-ec (:deque i (insert* (list 1 2 3) empty)) i)
             => (list 1 2 3))))

; empty?
(check (empty? empty) => #t)
(check (empty? (insert 1 empty)) => #f)

; elements 
;  - see insert

; first
(check (first (insert 1 empty)) => 1)

; first+remove
(check (let-values  ([(f r) (first+remove (insert* (list 1 2 3) empty))])
         (list f (elements r)))
       => (list 1 (list 2 3)))

; fold
(check (fold + 0 (insert* (list 1 2 3 4 5) empty)) => 15)
(check (fold + 0 empty) => 0)

; insert and insert-last
(define (check-insert ins)
  (check (elements (ins 1 empty)) => (list 1))
  (check (elements (ins 2 empty)) => (list 2))
  (check (elements (ins 1 (insert 2 empty))) => (list 2 1))
  (check (elements (ins 1 (insert 2 (insert 3 empty)))) => (list 3 2 1)))
(check-insert insert)
(check-insert insert-last)

; insert*
(check (elements (insert* '() empty)) => (list))
(check (elements (insert* (list 1) empty)) => (list 1))
(check (elements (insert* (list 1 2) empty)) => (list 1 2))
(check (elements (insert* (list 1 2 3) empty)) => (list 1 2 3))

; insert-first (only deques)
(if *deque*
    (begin
      (check (elements (insert-first 4 empty)) => (list 4))
      (check (elements (insert-first 4 (insert* (list 1 2 3) empty)))
             => (list 4 1 2 3))))

; remove and remove-first are synonyms
(define (check-remove rem)
  (check (elements (rem (insert 1 empty))) => '())
  (check (elements (rem (insert 2 empty))) => '())
  (check (elements (rem (insert 1 (insert 2 empty)))) => (list 1))
  (check (elements (rem (insert* (list 1 2 3 4 5 ) empty))) => (list 2 3 4 5)))
(check-remove remove)
(check-remove remove-first)

; remove-last
(if *deque*
    (begin
      (check (empty? (remove-last (remove-last (insert* (list 1 2) empty)))) => #t)
      (check (elements (remove-last (insert* (list 1 2 3 4 5) empty)))
             => (list 1 2 3 4))
      (check (elements (remove-last (insert* (list 1 2) empty)))
             => (list 1))
      (check (elements (remove-last (insert* (list 1) empty)))
             => (list))
      (check  (elements (remove-last (remove-last (insert-last 5 (insert-first 4 (insert-last 3 (insert* (list 1 2) empty)))))))
              => (list 4 1 2))))

; size
(check (size (insert* (list 1 2 3) empty)) => 3)
(check (size (insert* (list 1) empty)) => 1)
(check (size empty) => 0)

; queue?
(check (queue? empty) => #t)
(check (queue? (insert 1 empty)) => #t)
(check (queue? '()) => #f)
(check (queue? '(1)) => #f)

; queue-ec
(check (elements (queue-ec (: i 3) i))
       => (list 0 1 2))

; :queue and :
(check (list-ec (:queue i (insert* (list 1 2 3) empty)) i)
       => (list 1 2 3))
(check (list-ec (: i (insert* (list 1 2 3) empty)) i)
       => (list 1 2 3))

;;; REPORT
(check-report)
