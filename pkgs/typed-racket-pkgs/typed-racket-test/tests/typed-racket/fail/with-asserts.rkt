#;
(exn-pred exn:fail?)
#lang typed/racket

(let ([x 1] [y "2"])
  (with-asserts ([x string?] [y integer?])
                x))
