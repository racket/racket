#lang scheme/base

(require (planet schematics/schemeunit:3)
         (planet schematics/benchmark:2))

;; Test that comprehensions are as fast as hand-written
;; loops


;;
;; Vector comprehensions
;;

(define big-vector (make-vector 65536 1))

(test-case
 "simplest vector comprehension"
 (check-as-fast
  "comprehension"
  (lambda ()
    (for/fold ([sum 0]) ([x (in-vector big-vector)])
              (+ sum x)))
  "hand-written loop"
  (lambda ()
    (let ([end (vector-length big-vector)])
      (let loop ([i 0] [sum 0])
        (if (= i end)
            sum
            (loop (add1 i) (+ (vector-ref big-vector i) sum))))))))

(test-case
 "vector comprehension with step"
 (check-as-fast
  "comprehension"
  (lambda ()
    (for/fold ([sum 0]) ([x (in-vector big-vector 0 (vector-length big-vector) 2)])
              (+ sum x)))
  "hand-written loop"
  (lambda ()
    (let ([end (vector-length big-vector)])
      (let loop ([i 0] [sum 0])
        (if (= i end)
            sum
            (loop (+ i 2) (+ (vector-ref big-vector i) sum))))))))
