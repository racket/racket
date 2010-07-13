#lang typed/scheme 

(require (file "cross-module-struct.rkt") racket/unsafe/ops)
(define a (make-x 1))
(x-x a)
