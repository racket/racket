#lang typed/racket

(require (for-syntax syntax/parse)
         racket/unsafe/ops
         typed/rackunit
         racket/flonum)

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

;; ===================================================================================================
;; for/flvector:

(define-syntax-rule (test-flvector a b) ; for some reason, check-equal? doesn't work below
  (unless (equal? a b)
    (error "bad")))

(test-flvector (for/flvector: #:length 4 ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for/flvector: #:length 4 ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector 0.0 0.0 0.0 0.0))

(test-flvector (for/flvector: #:length 4 ()
                1.2)
              (flvector 1.2 0.0 0.0 0.0))

(test-flvector (for/flvector: #:length 4 ([i  (in-range 2)])
                (real->double-flonum i))
              (flvector 0.0 1.0 0.0 0.0))

(test-flvector (for/flvector: #:length 4 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for/flvector: #:length 0 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector))

(test-flvector (for/flvector: ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for/flvector: () 1.2)
              (flvector 1.2))

(test-flvector (for/flvector: ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector))

(test-flvector (for/flvector: #:length 4 ([x  (in-range 2)]
                                          #:when #t
                                          [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(test-flvector (for/flvector: #:length 4 ([x  (in-range 0)]
                                          #:when #t
                                          [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 0.0 0.0 0.0))

(test-flvector (for/flvector: #:length 4 ([x  (in-range 2)]
                                          #:when #t
                                          [y  (in-range 1)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 0.0 0.0))

(test-flvector (for/flvector: #:length 4 ([x  (in-range 2)]
                                          #:when #t
                                          [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 2.0 1.0))

(test-flvector (for/flvector: #:length 0 ([x  (in-range 2)]
                                          #:when #t
                                          [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector))

(test-flvector (for/flvector: ([x  (in-range 2)]
                               #:when #t
                               [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(test-flvector (for/flvector: ([x  (in-range 0)]
                               #:when #t
                               [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector))

;; ===================================================================================================
;; for*/flvector:

(test-flvector (for*/flvector: #:length 4 ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for*/flvector: #:length 4 ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector 0.0 0.0 0.0 0.0))

(test-flvector (for*/flvector: #:length 4 ()
                1.2)
              (flvector 1.2 0.0 0.0 0.0))

(test-flvector (for*/flvector: #:length 4 ([i  (in-range 2)])
                (real->double-flonum i))
              (flvector 0.0 1.0 0.0 0.0))

(test-flvector (for*/flvector: #:length 4 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for*/flvector: #:length 0 ([i  (in-range 5)])
                (real->double-flonum i))
              (flvector))

(test-flvector (for*/flvector: ([i  (in-range 4)])
                (real->double-flonum i))
              (flvector 0.0 1.0 2.0 3.0))

(test-flvector (for*/flvector: () 1.2)
              (flvector 1.2))

(test-flvector (for*/flvector: ([i  (in-range 0)])
                (real->double-flonum i))
              (flvector))

(test-flvector (for*/flvector: #:length 4 ([x  (in-range 2)]
                                           [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(test-flvector (for*/flvector: #:length 4 ([x  (in-range 0)]
                                           [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 0.0 0.0 0.0))

(test-flvector (for*/flvector: #:length 4 ([x  (in-range 2)]
                                           [y  (in-range 1)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 0.0 0.0))

(test-flvector (for*/flvector: #:length 4 ([x  (in-range 2)]
                                           [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 2.0 1.0))

(test-flvector (for*/flvector: #:length 0 ([x  (in-range 2)]
                                           [y  (in-range 3)])
                (real->double-flonum (+ x y)))
              (flvector))

(test-flvector (for*/flvector: ([x  (in-range 2)]
                                [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector 0.0 1.0 1.0 2.0))

(test-flvector (for*/flvector: ([x  (in-range 0)]
                                [y  (in-range 2)])
                (real->double-flonum (+ x y)))
              (flvector))
