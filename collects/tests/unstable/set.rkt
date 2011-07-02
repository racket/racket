#lang racket

(require rackunit rackunit/text-ui racket/set "helpers.rkt")

(define (check/set a-set a-list #:= [== equal?])
  (check/sort (set->list a-set) a-list #:= ==))

(define-syntax-rule (test/set arg ...)
  (test (check/set arg ...)))

(run-tests
 (test-suite "set.rkt"
   (test-suite "Conversions"
     (test-suite "list->set"
       (test/set (list->set (list 'a 'b 'c)) (list 'a 'b 'c))
       (test/set (list->set (list 'c 'b 'a)) (list 'a 'b 'c)))
     (test-suite "list->seteq"
       (test/set (list->seteq (list 'a 'b 'c)) (list 'a 'b 'c))
       (test/set (list->seteq (list 'c 'b 'a)) (list 'a 'b 'c)))
     (test-suite "list->seteqv"
       (test/set (list->seteqv (list 'a 'b 'c)) (list 'a 'b 'c))
       (test/set (list->seteqv (list 'c 'b 'a)) (list 'a 'b 'c)))
     (test-suite "set->list"
       (test (check/sort (set->list (set 1 2 3)) (list 1 2 3)))))
   (test-suite "Comparisons"
     (test-suite "set=?"
       (test (check-false (set=? (set 1) (set 1 2 3))))
       (test (check-false (set=? (set 1 2 3) (set 1))))
       (test (check-true (set=? (set 1 2 3) (set 1 2 3)))))
     (test-suite "proper-subset?"
       (test (check-true (proper-subset? (set 1) (set 1 2 3))))
       (test (check-false (proper-subset? (set 1 2 3) (set 1))))
       (test (check-false (proper-subset? (set 1 2 3) (set 1 2 3))))))
   (test-suite "Combinations"
     (test-suite "set-symmetric-difference"
       (test/set (set-symmetric-difference (set 1) (set 1 2) (set 1 2 3)) (list 1 3))
       (test/set (set-symmetric-difference (set 1) (set 2) (set 3)) (list 1 2 3))
       (test/set (set-symmetric-difference (set 1 2) (set 2 3) (set 1 3)) (list))))))
