#;
(exn-pred #rx"expected: 2 values\n  given: 1 value")
#lang typed/racket

;; This test ensures that the following snippet doesn't
;; result in an internal error.
;;
;; see
;; http://lists.racket-lang.org/dev/archive/2013-January/011614.html

(: f (-> (Values String (Vector Integer Integer))))
(define (f) (vector 1 2))

