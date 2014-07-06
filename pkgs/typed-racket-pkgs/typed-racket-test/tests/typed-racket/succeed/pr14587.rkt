#lang racket
(require typed/racket)
(with-type #:result Any
  (for/fold ([prod : Natural 1])
            ([x : Natural '(1 2 3 4 5)])
    (* x prod)))

