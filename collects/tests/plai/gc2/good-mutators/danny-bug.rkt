#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 8)

(define
  proc
  (let* ([not-root 1] ; 2
         [root 2]) ; 4
    (lambda () ; 8
      3
      root)))

(proc)
