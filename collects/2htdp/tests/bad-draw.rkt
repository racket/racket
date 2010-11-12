#lang scheme

(require 2htdp/universe)

(define s "")
(define x 0)

(with-handlers ((exn? (lambda _ "success!")))
  (big-bang 0
            (on-tick (lambda (w) (begin (set! x (+ x 1)) w)))
            (to-draw (lambda (w) (set! s (number->string w))))))
          
