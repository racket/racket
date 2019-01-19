#lang racket/base
(require "../../common/check.rkt"
         (for-syntax racket/base))

(provide (all-from-out "../../common/check.rkt")
         check-range
         check-immutable-field)

(define (check-range who start-pos end-pos max-end in-value)
  (when (start-pos . > . max-end)
    (raise-range-error who
                       "byte string"
                       "starting "
                       start-pos
                       in-value
                       0
                       max-end
                       #f))
  (when (or (end-pos . < . start-pos)
            (end-pos . > . max-end))
    (raise-range-error who
                       "byte string"
                       "ending "
                       end-pos
                       in-value
                       0
                       max-end
                       start-pos)))

(define (check-immutable-field who v sti)
  (when (exact-integer? v)
    (unless (memv v (list-ref sti 5))
      (raise-arguments-error who "field index not declared immutable"
                             "field index" v))))
