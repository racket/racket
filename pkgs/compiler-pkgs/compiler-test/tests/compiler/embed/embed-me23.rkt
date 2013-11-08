#lang racket/base
(require racket/serialize)

(serializable-struct foo (a b))

(define f (deserialize (serialize (foo 1 2))))
(foo-a f)
(foo-b f)
