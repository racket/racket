;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname elevator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/elevator)

;; next3 : (union 'up 'down) N X -> N
;; always sends elevator to next floor up or down, 
;; switches direction at either end 
(define (next3 x y z)
  (cond
    ((and (eq? 'up x) (< y 8)) (+ y 1))
    ((eq? 'up x) 7) ; anything down
    ((and (eq? 'down x) (> y 1)) (- y 1))
    (else 2))) ; anything up

(run next3)
