#lang scheme
(require unstable/list)
(require tests/eli-tester)
(test
 (remf positive? '()) => '()
 (remf positive? '(1 -2 3 4 -5)) => '(-2 3 4 -5)
 (remf even? '(1 -2 3 4 -5)) => '(1 3 4 -5)
 (remf (λ (x) #f) '(1 -2 3 4 -5)) => '(1 -2 3 4 -5))

(require rackunit rackunit/text-ui)

(run-tests
 (test-suite "unstable/list"
   (test-suite "map2"
     (test-case "numerator and denominator"
       (let*-values ([(ns ds)
                      (map2
                       (lambda (r)
                         (values (numerator r) (denominator r)))
                       (list 1/2 3/4 5/6))])
         (check-equal? (list ns ds) (list '(1 3 5) '(2 4 6))))))
   (test-suite "map/values"
     (test-case "complex numerator and denominator"
       (let*-values ([(rns rds ins ids)
                      (map/values
                       4
                       (lambda (c)
                         (values (numerator (real-part c))
                                 (denominator (real-part c))
                                 (numerator (imag-part c))
                                 (denominator (imag-part c))))
                       (list 1/2+3/4i 5/6+7/8i))])
         (check-equal? (list rns rds ins ids)
                       (list '(1 5) '(2 6) '(3 7) '(4 8)))))
     (test-case "multiple lists"
       (let*-values ([(as bs cs)
                      (map/values 3 values '(1 2 3) '(4 5 6) '(7 8 9))])
         (check-equal? as '(1 2 3))
         (check-equal? bs '(4 5 6))
         (check-equal? cs '(7 8 9)))))
   (test-suite "group-by"
     (check-equal? (group-by values '(1 4 2 56 2 3))
                   '((3) (56) (2 2) (4) (1)))
     (check-equal? (group-by values '(1 2 1 2 54 2 5 43 7 2 643 1 2 0))
                   '((0) (2 2 2 2 2) (7) (43) (5) (54) (643) (1 1 1)))
     (check-equal? (group-by values
                             '(1 4 2 56 2 3)
                             (lambda (x y) (or (and (even? x) (even? y))
                                               (and (odd?  x) (odd?  y)))))
                   '((4 2 56 2) (1 3)))
     (check-equal? (group-by car '((1 a) (4 b) (2 c) (56 d) (2 e) (3 f)))
                    '(((3 f)) ((56 d)) ((2 c) (2 e)) ((4 b)) ((1 a))))
     (check-equal? (group-by even? '(1 2 3 4 5 6))
                   '((2 4 6) (1 3 5))))
   (test-suite "cartesian-product"
     (check-equal? (cartesian-product '(1 2 3) '(a b c))
                   '((1 a) (1 b) (1 c)
                     (2 a) (2 b) (2 c)
                     (3 a) (3 b) (3 c)))
     (check-equal? (cartesian-product '(4 5 6) '(d e f) '(#t #f))
                   '((4 d #t) (4 d #f) (4 e #t) (4 e #f) (4 f #t) (4 f #f)
                     (5 d #t) (5 d #f) (5 e #t) (5 e #f) (5 f #t) (5 f #f)
                     (6 d #t) (6 d #f) (6 e #t) (6 e #f) (6 f #t) (6 f #f))))))
