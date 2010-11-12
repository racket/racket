#lang racket

(require racket/serialize
         "impl-eval.rkt")

(define timeout (read))
(define bytecode (deserialize (read)))

(write (serialize (eval-impl bytecode timeout)))
