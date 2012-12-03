#lang racket

(require math/array
         rackunit)

;; Make a procedure that returns a random value to keep the optimizer from converting it to a
;; top-level, non-closure; if that happens, the module keeps a reference to it, which makes this
;; test always fail
(define bx (make-weak-box (let ([v  (random)]) (λ (js) v))))

(define arr (build-array #() (weak-box-value bx)))

;; Making `arr' strict should release the only remaining reference to the contents of `bx'
(array-strict! arr)
(collect-garbage)
(check-false (weak-box-value bx))
