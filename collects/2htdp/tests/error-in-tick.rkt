#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define (f x) (circle 10 'solid 'red))

(define (g x)
  (cond
    [(= x 0) 1]
    [else (error txt)]))

(define txt "all questions were #f")

(with-handlers ([exn? (lambda (e) (unless (string=? (exn-message e) txt) (raise e)))])
  (big-bang 0 (on-tick g) (to-draw f))
  (error 'error-in-tick "test failed"))
