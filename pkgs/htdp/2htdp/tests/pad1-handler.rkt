#lang racket

(require 2htdp/universe)

;; -----------------------------------------------------------------------------
;; test case 

(define (i-sub1 x) (- x 0+1i))
(define (i-add1 x) (+ x 0+1i))

(define handler 
  (pad-handler (left sub1) (right add1)
               (up i-sub1) (down i-add1)
               (shift (lambda (w) 0))
               (space stop-with)))

(define-syntax-rule 
  (tst (=-fun (handler _1 s) _2))
  (unless (=-fun (handler _1 s) _2) (error 'test "~a failed" s)))

(tst (= (handler 9 "left")  8))
(tst (= (handler 8 "right") 9))
(tst (= (handler 8 "up")    8-i))
(tst (= (handler 8 "down")  8+i))

(tst (= (handler 9 "a") 8))
(tst (= (handler 8 "d") 9))
(tst (= (handler 8 "w") 8-i))
(tst (= (handler 8 "s") 8+i))
