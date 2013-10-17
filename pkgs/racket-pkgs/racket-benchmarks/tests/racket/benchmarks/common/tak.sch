;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         tak.sch
; Description:  TAK benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 09:58:18 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; TAK -- A vanilla version of the TAKeuchi function

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))
 
;;; call: (tak 18 12 6)

(let ((input (with-input-from-file "input.txt" read)))
  (time
   (let loop ((n 15000) (v 0))
     (if (zero? n)
         v
         (loop (- n 1) (tak 18 12 (if input 6 0)))))))
