#;
(exn-pred exn:fail:syntax?)
#lang typed/racket

;; don't allow duplicate type variable names

(plambda: (a a a) ([x : a]) x)
(popt-lambda: (a a a) ([x : a]) x)
(pcase-lambda: (a a a) ([x : a]) x)
