#lang racket 

;; ---------------------------------------------------------------------------------------------------
;; does big-bang stop when the initial world is already a final world? does it draw the final image? 

(require 2htdp/universe)
(require 2htdp/image)

(define ((draw message) x)
  (display message) 
  (circle 3 'solid 'red))

(define-syntax-rule 
  (test body expected-value expected-output)
  (begin
    (define actual-value (gensym))
    (define actual-output (with-output-to-string (lambda () (set! actual-value body))))
    (unless (equal? actual-value expected-value)
      (error 'failure "~a expected value ~e, value produced ~e" 'test expected-value actual-value))
    (unless (string=? actual-output expected-output)
      (error 'failure "~a expected output ~e, output produced ~e" 'test expected-output actual-output))))

(test (big-bang 0 (stop-when zero?) (on-tick add1) (to-draw (draw ""))) 0 "")

(test (big-bang (stop-with 0) (on-tick add1) (to-draw (draw ""))) 0 "")

(test (big-bang 0 (on-draw (draw 0)) (stop-when zero? (draw 1))) 0 "1")

