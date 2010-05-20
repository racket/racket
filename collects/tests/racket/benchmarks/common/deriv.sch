;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         deriv.sch
; Description:  The DERIV benchmark from the Gabriel tests.
; Author:       Vaughan Pratt
; Created:      8-Apr-85
; Modified:     10-Apr-85 14:53:50 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
;               9-Feb-88 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; DERIV -- Symbolic derivative benchmark written by Vaughn Pratt.
;;; It uses a simple subset of Lisp and does a lot of  CONSing.

; Returns the wrong answer for quotients.
; Fortunately these aren't used in the benchmark.

(define (deriv-aux a) (list '/ (deriv a) a))
 
(define (deriv a)
  (cond
    ((not (pair? a))
     (cond ((eq? a 'x) 1) (else 0)))
    ((eq? (car a) '+)
     (cons '+ (map deriv (cdr a))))
    ((eq? (car a) '-)
     (cons '- (map deriv
                      (cdr a))))
    ((eq? (car a) '*)
     (list '*
           a
           (cons '+ (map deriv-aux (cdr a)))))
    ((eq? (car a) '/)
     (list '-
           (list '/
                 (deriv (cadr a))
                 (caddr a))
           (list '/
                 (cadr a)
                 (list '*
                       (caddr a)
                       (caddr a)
                       (deriv (caddr a))))))
    (else 'error)))
 
(define (run)
  (do ((i 0 (+ i 1)))
      ((= i 1000000))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))
 
;;; call:  (run)
 
(time (run))

