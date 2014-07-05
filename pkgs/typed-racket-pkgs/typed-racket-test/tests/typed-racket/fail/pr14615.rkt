#lang typed/racket/base
(require racket/gui)
(define (f)
  (let ([fn (put-file)])
    (when fn
      (values fn))))
