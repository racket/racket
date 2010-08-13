#lang racket
(require racket/unsafe/ops)
(let ([f*1 add1])
  (let loop2 ([v3 0] [lst4 (list 1 2 3)])
    (if (null? lst4)
        v3
        (loop2 (+ v3 (f*1 (unsafe-car lst4))) (unsafe-cdr lst4)))))
(let ([f*1 add1])
  (let loop2 ([v3 1] [lst4 (list 1 2 3)])
    (if (null? lst4)
        v3
        (loop2 (* v3 (f*1 (unsafe-car lst4))) (unsafe-cdr lst4)))))
(void)
