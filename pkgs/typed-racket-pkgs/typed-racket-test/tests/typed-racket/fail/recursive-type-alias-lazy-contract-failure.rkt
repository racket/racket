#;
(exn-pred #rx"Type Bar could not.*variable arity polymorphic type")
#lang typed/racket

;; Check that contracts generation failure for mutually recursive type
;; is delayed until the types are actually used for an export.

(define-type Foo (All (x ...) (-> x ... x Bar)))
(define-type Bar (U #f (Listof Foo)))

(: f (-> Bar Void))
(define (f x) (void))
(provide f)
