#lang typed/racket

(require math/array
         math/matrix
         typed/rackunit)

(let ()
  (define arr (list->array number? '[[1 2]
                                     [3 4]]))
  (define brr (list->array number? '[[10 20]
                                     [30 40]]))
  (array-strict (matrix* arr brr)))

[list [list (+ (* 1 10) (* 2 30)) (+ (* 1 20) (* 2 40))]
      [list (+ (* 3 10) (* 4 30)) (+ (* 3 20) (* 4 40))]]

(let ()
  (define arr (list->array number? '[[1 2 3]
                                     [4 5 6]]))
  (define brr (list->array number? '[[10 20]
                                     [30 40]
                                     [50 60]]))
  (array-strict (matrix* arr brr)))

[list [list (+ (* 1 10) (* 2 30) (* 3 50)) (+ (* 1 20) (* 2 40) (* 3 60))]
      [list (+ (* 4 10) (* 5 30) (* 6 50)) (+ (* 4 20) (* 5 40) (* 6 60))]]

(let ()
  (define arr ((inst make-strict-array Number)
               '(100 100)
               (build-vector (* 100 100) (λ: ([n : Number]) n))))
  (define brr ((inst make-strict-array Number)
               '(100 100)
               (build-vector (* 100 100) (λ: ([n : Number]) n))))
  (for ([_  (in-range 5)])
    (time (for ([_  (in-range 10)])
            (array-strict (matrix* arr brr))))))

#;(let ()
  (define arr ((inst make-strict-array Float)
               '(100 100)
               (build-vector (* 100 100) (λ: ([n : Real]) (real->double-flonum n)))))
  (define brr ((inst make-strict-array Float)
               '(100 100)
               (build-vector (* 100 100) (λ: ([n : Real]) (real->double-flonum n)))))
  (for ([_  (in-range 5)])
      (time (for ([_  (in-range 10)])
              (array-strict (matrix-fl* arr brr))))))
