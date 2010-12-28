#lang scheme

(require picturing-programs)

(define s "")
(define x 0)

(with-handlers ((exn? void))
  (big-bang 0
            (on-tick (lambda (w) (begin (set! x (+ x 1)) w)))
            (on-draw (lambda (w) (set! s (number->string w))))))
          
