(+ 14
   (local
       ((define (fact x)
          (if (= x 0)
              1
              (* x (fact (- x 1)))))
        (define nother (lambda (x) x))
        (define a (+ 3 5))
        (define b (+ a 13)))
     (fact b)))