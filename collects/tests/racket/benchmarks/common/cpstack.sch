;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         cpstak.sch
; Description:  continuation-passing version of TAK
; Author:       Will Clinger
; Created:      20-Aug-87
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  (tak x y z (lambda (a) a)))
 
;;; call: (cpstak 18 12 6)
 
(time (cpstak 18 12 2))
