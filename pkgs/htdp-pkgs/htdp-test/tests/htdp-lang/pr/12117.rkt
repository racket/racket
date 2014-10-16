#lang htdp/asl

(require racket/match)
(define-struct a (b))
 (match (make-a 1) [(struct a (b)) b] [#f 3])
