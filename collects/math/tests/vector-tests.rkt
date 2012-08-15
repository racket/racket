#lang typed/racket

(require racket/flonum
         math/flonum
         math/vector
         typed/rackunit)

;; ===================================================================================================
;; for/flvector:

(check-equal? (for/flvector: #:length 4 ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for/flvector: #:length 4 ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector 0.0 0.0 0.0 0.0))

(check-equal? (for/flvector: #:length 4 ()
                1.2)
              (flvector 1.2 0.0 0.0 0.0))

(check-equal? (for/flvector: #:length 4 ([i  (in-range 2)])
                (real->double-flonum i))
              (flvector 0.0 1.0 0.0 0.0))

(check-equal? (for/flvector: #:length 4 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for/flvector: #:length 0 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector))

(check-equal? (for/flvector: ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for/flvector: () 1.2)
              (flvector 1.2))

(check-equal? (for/flvector: ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector))

(check-equal? (for/flvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(check-equal? (for/flvector: #:length 4 ([x  (in-range 0)]
                                         #:when #t
                                         [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 0.0 0.0 0.0))

(check-equal? (for/flvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 1)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 0.0 0.0))

(check-equal? (for/flvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 2.0 1.0))

(check-equal? (for/flvector: #:length 0 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector))

(check-equal? (for/flvector: ([x  (in-range 2)]
                              #:when #t
                              [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(check-equal? (for/flvector: ([x  (in-range 0)]
                              #:when #t
                              [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector))

;; ===================================================================================================
;; for*/flvector:

(check-equal? (for*/flvector: #:length 4 ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for*/flvector: #:length 4 ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector 0.0 0.0 0.0 0.0))

(check-equal? (for*/flvector: #:length 4 ()
                1.2)
              (flvector 1.2 0.0 0.0 0.0))

(check-equal? (for*/flvector: #:length 4 ([i  (in-range 2)])
                (real->double-flonum i))
              (flvector 0.0 1.0 0.0 0.0))

(check-equal? (for*/flvector: #:length 4 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for*/flvector: #:length 0 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector))

(check-equal? (for*/flvector: ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(check-equal? (for*/flvector: () 1.2)
              (flvector 1.2))

(check-equal? (for*/flvector: ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector))

(check-equal? (for*/flvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(check-equal? (for*/flvector: #:length 4 ([x  (in-range 0)]
                                          [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 0.0 0.0 0.0))

(check-equal? (for*/flvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 1)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 0.0 0.0))

(check-equal? (for*/flvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 2.0 1.0))

(check-equal? (for*/flvector: #:length 0 ([x  (in-range 2)]
                                          [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector))

(check-equal? (for*/flvector: ([x  (in-range 2)]
                               [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(check-equal? (for*/flvector: ([x  (in-range 0)]
                               [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector))

;; ===================================================================================================
;; for/fcvector:

(check-equal? (for/fcvector: #:length 4 ([i  (in-range 4)])
                             (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([i  (in-range 0)])
                             (number->float-complex i))
              (fcvector 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ()
                             1.2+0.0i)
              (fcvector 1.2+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([i  (in-range 2)])
                             (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([i  (in-range 5)])
                             (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for/fcvector: #:length 0 ([i  (in-range 5)])
                             (number->float-complex i))
              (fcvector))

(check-equal? (for/fcvector: ([i  (in-range 4)])
                             (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for/fcvector: () 1.2+0.0i)
              (fcvector 1.2+0.0i))

(check-equal? (for/fcvector: ([i  (in-range 0)])
                             (number->float-complex i))
              (fcvector))

(check-equal? (for/fcvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 2)])
                             (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 1.0+0.0i 2.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([x  (in-range 0)]
                                         #:when #t
                                         [y  (in-range 2)])
                             (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 1)])
                             (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for/fcvector: #:length 4 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 3)])
                             (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 1.0+0.0i))

(check-equal? (for/fcvector: #:length 0 ([x  (in-range 2)]
                                         #:when #t
                                         [y  (in-range 3)])
                             (number->float-complex (+ x y)))
              (fcvector))

(check-equal? (for/fcvector: ([x  (in-range 2)]
                              #:when #t
                              [y  (in-range 2)])
                             (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 1.0+0.0i 2.0+0.0i))

(check-equal? (for/fcvector: ([x  (in-range 0)]
                              #:when #t
                              [y  (in-range 2)])
                             (number->float-complex (+ x y)))
              (fcvector))

;; ===================================================================================================
;; for*/fcvector:

(check-equal? (for*/fcvector: #:length 4 ([i  (in-range 4)])
                              (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([i  (in-range 0)])
                              (number->float-complex i))
              (fcvector 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ()
                              1.2+0.0i)
              (fcvector 1.2+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([i  (in-range 2)])
                              (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([i  (in-range 5)])
                              (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for*/fcvector: #:length 0 ([i  (in-range 5)])
                              (number->float-complex i))
              (fcvector))

(check-equal? (for*/fcvector: ([i  (in-range 4)])
                              (number->float-complex i))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 3.0+0.0i))

(check-equal? (for*/fcvector: () 1.2+0.0i)
              (fcvector 1.2+0.0i))

(check-equal? (for*/fcvector: ([i  (in-range 0)])
                              (number->float-complex i))
              (fcvector))

(check-equal? (for*/fcvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 2)])
                              (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 1.0+0.0i 2.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([x  (in-range 0)]
                                          [y  (in-range 2)])
                              (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 1)])
                              (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 0.0+0.0i 0.0+0.0i))

(check-equal? (for*/fcvector: #:length 4 ([x  (in-range 2)]
                                          [y  (in-range 3)])
                              (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 2.0+0.0i 1.0+0.0i))

(check-equal? (for*/fcvector: #:length 0 ([x  (in-range 2)]
                                          [y  (in-range 3)])
                              (number->float-complex (+ x y)))
              (fcvector))

(check-equal? (for*/fcvector: ([x  (in-range 2)]
                               [y  (in-range 2)])
                              (number->float-complex (+ x y)))
              (fcvector 0.0+0.0i 1.0+0.0i 1.0+0.0i 2.0+0.0i))

(check-equal? (for*/fcvector: ([x  (in-range 0)]
                               [y  (in-range 2)])
                              (number->float-complex (+ x y)))
              (fcvector))

;; ===================================================================================================
;; in-fcvector

(check-equal? (sequence->list (in-fcvector (list->fcvector '())))
              '())

(check-equal? (sequence->list (in-fcvector (list->fcvector '(1 2 3 4))))
              (map number->float-complex '(1 2 3 4)))

(check-equal? (for/fcvector: ([z  (in-fcvector (list->fcvector '()))]) z)
              (list->fcvector '()))

(check-equal? (for/fcvector: ([z  (in-fcvector (list->fcvector '(1 2 3 4)))]) z)
              (list->fcvector '(1 2 3 4)))

;; ===================================================================================================
;; for/vector:

(check-equal? (for/vector: #:length 4 ([i  (in-range 4)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-exn exn? (λ () (for/vector: #:length 4 ([i  (in-range 0)]) : Float
                        (real->double-flonum i))))

(check-equal? (for/vector: #:length 4 () : Float
                1.2)
              (vector 1.2 1.2 1.2 1.2))

(check-equal? (for/vector: #:length 4 ([i  (in-range 2)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 0.0 0.0))

(check-equal? (for/vector: #:length 4 ([i  (in-range 5)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-equal? (for/vector: #:length 0 ([i  (in-range 5)]) : Float
                (real->double-flonum i))
              (vector))

(check-equal? (for/vector: ([i  (in-range 4)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-equal? (for/vector: () : Float 1.2)
              (vector 1.2))

(check-equal? (for/vector: ([i  (in-range 0)]) : Float
                (real->double-flonum i))
              (vector))

(check-equal? (for/vector: #:length 4 ([x  (in-range 2)]
                                       #:when #t
                                       [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 1.0 2.0))

(check-exn exn? (λ () (for/vector: #:length 4 ([x  (in-range 0)]
                                               #:when #t
                                               [y  (in-range 2)]) : Float
                        (real->double-flonum (+ x y)))))

(check-equal? (for/vector: #:length 4 ([x  (in-range 2)]
                                       #:when #t
                                       [y  (in-range 1)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 0.0 0.0))

(check-equal? (for/vector: #:length 4 ([x  (in-range 2)]
                                       #:when #t
                                       [y  (in-range 3)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 2.0 1.0))

(check-equal? (for/vector: #:length 0 ([x  (in-range 2)]
                                       #:when #t
                                       [y  (in-range 3)]) : Float
                (real->double-flonum (+ x y)))
              (vector))

(check-equal? (for/vector: ([x  (in-range 2)]
                            #:when #t
                            [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 1.0 2.0))

(check-equal? (for/vector: ([x  (in-range 0)]
                            #:when #t
                            [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector))

;; ===================================================================================================
;; for*/vector:

(check-equal? (for*/vector: #:length 4 ([i  (in-range 4)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-exn exn? (λ () (for*/vector: #:length 4 ([i  (in-range 0)]) : Float
                        (real->double-flonum i))))

(check-equal? (for*/vector: #:length 4 () : Float
                1.2)
              (vector 1.2 1.2 1.2 1.2))

(check-equal? (for*/vector: #:length 4 ([i  (in-range 2)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 0.0 0.0))

(check-equal? (for*/vector: #:length 4 ([i  (in-range 5)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-equal? (for*/vector: #:length 0 ([i  (in-range 5)]) : Float
                (real->double-flonum i))
              (vector))

(check-equal? (for*/vector: ([i  (in-range 4)]) : Float
                (real->double-flonum i))
              (vector 0.0 1.0 2.0 3.0))

(check-equal? (for*/vector: () : Float 1.2)
              (vector 1.2))

(check-equal? (for*/vector: ([i  (in-range 0)]) : Float
                (real->double-flonum i))
              (vector))

(check-equal? (for*/vector: #:length 4 ([x  (in-range 2)]
                                        [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 1.0 2.0))

(check-exn exn? (λ () (for*/vector: #:length 4 ([x  (in-range 0)]
                                                [y  (in-range 2)]) : Float
                        (real->double-flonum (+ x y)))))

(check-equal? (for*/vector: #:length 4 ([x  (in-range 2)]
                                        [y  (in-range 1)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 0.0 0.0))

(check-equal? (for*/vector: #:length 4 ([x  (in-range 2)]
                                        [y  (in-range 3)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 2.0 1.0))

(check-equal? (for*/vector: #:length 0 ([x  (in-range 2)]
                                        [y  (in-range 3)]) : Float
                (real->double-flonum (+ x y)))
              (vector))

(check-equal? (for*/vector: ([x  (in-range 2)]
                             [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector 0.0 1.0 1.0 2.0))

(check-equal? (for*/vector: ([x  (in-range 0)]
                             [y  (in-range 2)]) : Float
                (real->double-flonum (+ x y)))
              (vector))
