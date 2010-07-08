#lang typed/scheme #:optimize

(require (file "cross-module-struct.rkt") racket/unsafe/ops)
(define a (make-x 1))
(unsafe-struct-ref a 0)
