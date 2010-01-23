#lang scheme/gui

(require 2htdp/universe)
(require 2htdp/image)

(define s "")
(define x 1)

(big-bang 1
          (on-tick (lambda (w)
                     (begin 
                       (set! x (+ x 1))
                       (if (= x 3) 0 1))))
          (stop-when zero?)
          (on-draw (lambda (w)
                     (begin
                       (set! s (string-append "-" s))
                       (rectangle 1 1 'solid 'green)))))

(unless (string=? s "---") (error 'world-update-test "failed! ~s" s))
