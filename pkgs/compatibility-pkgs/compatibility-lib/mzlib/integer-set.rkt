#lang racket/base

;; deprecated library, see data/integer-set

(require data/integer-set)
(provide (except-out (all-from-out data/integer-set)
                     subtract
                     symmetric-difference
                     count)
         (rename-out [subtract difference]
                     [symmetric-difference xor]
                     [count card]))
