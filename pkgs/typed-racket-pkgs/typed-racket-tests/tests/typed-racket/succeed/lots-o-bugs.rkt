#lang typed-scheme

;; (All (a ...) ( -> (a ... a -> Integer)))

(plambda: (a ...) ()
          (lambda: [ys : a ... a] 3))

(define x (plambda: (a ...) () (lambda: [ys : a ... a] 3)))



(: y (All (a ...) ( -> (a ... a -> Integer))))
(define y (plambda: (a ...) () (lambda: [ys : a ... a] 3)))

(: z (All (a ...) ( -> (a ... a -> Integer))))
(define z (lambda () (lambda ys 3)))

#;((plambda: (a ...) () (lambda: [ys : a ... a] 3)))

#;((plambda: (a ...) [xs : a ... a] (lambda: [ys : a ... a] 3))
   1 2 3 "foo")
