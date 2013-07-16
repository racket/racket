(define (squar x) (* x x))

(define (square-all list-of-numbers)
  (map squar list-of-numbers))

(define (my-map a-list)
  (cond [(empty? a-list) empty]
        [else (cons (square-all (car a-list))
                    (my-map (cdr a-list)))]))

(my-map (cons (cons 1 (cons 3 (cons 14 empty)))
              (cons (cons 3 (cons 4 empty))
                    (cons (cons 43 empty)
                          empty))))
