#lang typed-scheme

(let: ([x : (Boxof Number) (box "foo")]) x)

(let ()
  (: x (Boxof Number))
  (define x (box "foo"))
  x)
