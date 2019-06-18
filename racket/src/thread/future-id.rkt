#lang racket/base

(provide get-next-id)

(define ID (box 1))

(define get-next-id
  (lambda ()
    (let ([id (unbox ID)])
      (if (box-cas! ID id (+ 1 id))
          id
          (get-next-id)))))
