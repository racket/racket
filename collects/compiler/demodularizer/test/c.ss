#lang scheme/base
(require "a.ss"
         "b.ss")
(define c (+ a b))
(provide c)
c