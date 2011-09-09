#;
(exn-pred exn:fail?)
#lang typed/racket

(let ([x #f])
  (with-asserts ([x])
                x))
