#lang typed/racket

(require typed-racket/base-env/extra-procs)

((inst map Number Number Number Number Number Number Number)
 +
 (list 1 2 3) (list 2 3 4) (list 1 2 3) (list 2 3 4) (list 1 2 3) (list 2 3 4))
