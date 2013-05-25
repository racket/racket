#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 6)

(define
  proc
  (let* ([not-root 1] ; 2
         [root 2]) ; 4
    (lambda () ; 6
      3
      root)))

(proc)
