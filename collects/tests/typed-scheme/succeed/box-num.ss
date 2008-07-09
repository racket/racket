#lang typed-scheme

(let: ([x : (Boxof Number) (box 1)]) x)

(let ()
  (: x (Boxof Number))
  (define x (box 1))
  x)
