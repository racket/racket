#lang scheme

(require 2htdp/universe)

(define txt "expected to return a scene but this is a string")

(with-handlers ((exn? (lambda (e) (unless (string=? (exn-message e) txt) (raise e)))))
  (big-bang 0
            (on-tick add1)
            (to-draw (lambda (w) (error txt)))))
          
