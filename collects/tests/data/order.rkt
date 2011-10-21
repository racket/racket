#lang racket/base
(require rackunit
         data/order)

;; for tests
(struct fish (kind) #:transparent)
(struct fowl (kind) #:transparent)

;; datum-order tests

(define-syntax-rule (t cmp x y)
  (test-case (format "~s" '(t cmp x y))
    (check-equal? (datum-order x y) 'cmp)))

(t = 1 1)
(t = +inf.0 +inf.0)
(t = 8.0 8.0)
(t = +nan.0 +nan.0)
(t = +nan.0 (- +inf.0 +inf.0))
(t = 'apple 'apple)
(t = '(a #:b c) '(a #:b c))
(t = "apricot" "apricot")
(t = '#(1 2 3) '#(1 2 3))
(t = (box 'car) (box 'car))
(t = (box 'car) '#&car)
(t = '#s(point a 1) '#s(point a 1))
(t = (fish 'alewife) (fish 'alewife))

(t < 1 2)
(t > 8.0 5.0)
(t < 'apple 'candy)
(t < '(a #:b c) '(a #:c d c))
(t > '(5 . 4) '(3 2 1))
(t < '(a b . c) '(a b . z))
(t > "apricot" "apple")
(t > '#(1 2 3) '#(1 2))
(t < '#(1 2 3) '#(1 3))
(t > (box 'car) (box 'candy))
(t < '#s(point a 1) '#s(point b 0))
(t < '#s(A 1 2) '#s(Z 3 4 5))
(t < (fish 'alewife) (fish 'sockeye))

(define-syntax-rule (tc x y)
  (test-case (format "~s" '(tc x y))
    (let ([xy (datum-order x y)]
          [xy2 (datum-order x y)]
          [yx (datum-order y x)]
          [xy3 (datum-order x y)])
      ;; check consistency across multiple runs
      (check-equal? xy xy2)
      (check-equal? xy xy3)
      ;; check oppositeness
      (check member (list xy yx) '((< >) (> <))))))

(tc 1 2.0)
(tc 3+5i 3+2i)
(tc 'apple "zucchini")
(tc '(a b) '(a b . c))
(tc 0 'zero)

(tc (fish 'alewife) (fowl 'dodo))

(tc (fish 'alewife)
    (let ()
      (struct fish (x))
      (fish 'alewife)))
