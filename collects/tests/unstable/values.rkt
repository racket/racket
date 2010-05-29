#lang racket

(require rackunit rackunit/text-ui unstable/values "helpers.rkt")

(run-tests
 (test-suite "values.ss"
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
   (test-suite "foldl/values"
     (test-case "sum, product, and last"
       (let*-values ([(sum prod last)
                      (foldl/values
                       (lambda (next sum prod last)
                         (values (+ next sum)
                                 (* next prod)
                                 next))
                       (list 0 1 #f)
                       (list 1 2 3 4))])
         (check-equal? (list sum prod last)
                       (list 10 24 4)))))
   (test-suite "foldr/values"
     (test-case "sum, product, and last"
       (let*-values ([(sum prod last)
                      (foldr/values
                       (lambda (next sum prod last)
                         (values (+ next sum)
                                 (* next prod)
                                 next))
                       (list 0 1 #f)
                       (list 1 2 3 4))])
         (check-equal? (list sum prod last)
                       (list 10 24 1)))))
   (test-suite "values->list"
     (test-case "1 2 3 4"
       (check-equal? (values->list (values 1 2 3 4)) (list 1 2 3 4))))))
