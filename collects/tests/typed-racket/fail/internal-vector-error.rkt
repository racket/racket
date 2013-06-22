#;
(exn-pred #rx"Expected 2 values, but got 1")
#lang typed/racket

;; This test ensures that the following snippet doesn't
;; result in an internal error.
;;
;; see
;; http://lists.racket-lang.org/dev/archive/2013-January/011614.html

(: f (-> (Values String (Vector Integer Integer))))
(define (f) (vector 1 2))

