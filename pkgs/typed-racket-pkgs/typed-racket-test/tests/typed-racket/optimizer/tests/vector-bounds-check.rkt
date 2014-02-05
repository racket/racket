#;#;
#<<END
TR opt: vector-bounds-check.rkt 15:0 (vector-ref (make-my-vector) 0) -- vector partial bounds checking elimination
TR opt: vector-bounds-check.rkt 16:0 (flvector-ref (make-my-flvector) (ann 0 Fixnum)) -- flvector partial bounds checking elimination
END
#<<END
1.0
1.0

END
#lang typed/racket
(require racket/flonum)
#reader tests/typed-racket/optimizer/reset-port

(define: been-there-vector?   : Boolean #f)
(define: been-there-flvector? : Boolean #f)
(define (make-my-vector)
  (if been-there-vector?
      (error "make-my-vector should only be called once!")
      (set! been-there-vector? #t))
  (ann (vector 1.0 2.0 3.0) (Vectorof Flonum)))
(define (make-my-flvector)
  (if been-there-flvector?
      (error "make-my-flvector should only be called once!")
      (set! been-there-flvector? #t))
  (flvector 1.0 2.0 3.0))

(vector-ref (make-my-vector) 0)
(flvector-ref (make-my-flvector) (ann 0 Fixnum))
