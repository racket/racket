#lang typed/racket

;; Test a variance regression

(struct: (A) Foo ([elems : (Vectorof A)]))

(: make-foo (All (A) (A -> (Foo A))))
(define (make-foo x) (Foo (vector x)))

;; Need a module+ here because this test failed
;; originally when variance information wasn't preserved
;; across modules.
(module+ test
  ;; should type check, but won't if the element has
  ;; the Any type incorrectly
  (+ 1 (vector-ref (Foo-elems (make-foo 1)) 0)))
