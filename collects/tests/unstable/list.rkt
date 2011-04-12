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
         (check-equal? cs '(7 8 9)))))))
