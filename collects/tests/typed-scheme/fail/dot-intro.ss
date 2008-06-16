#lang typed-scheme

(plambda: (a ...) ([x : Number] . [y : Number ... a])
  y)

(plambda: (a ...) ([x : Number] . [y : Number ... a])
  (map add1 y))