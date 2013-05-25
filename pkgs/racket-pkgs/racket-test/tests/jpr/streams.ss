#lang scheme

#|
(define-syntax scons                                  ; SICP ==> ERROR (see Rationale of SRFI-41)
  (syntax-rules ()
    ((scons obj s) (cons obj (delay s)))))

(define (scar s) (car s))
(define (scdr s) (force (cdr s)))
|#

(define-syntax scons
  (syntax-rules ()
    ((scons obj s) (delay (cons obj (delay s))))))    ; from my book

(define (scar s) (car (force s)))
(define (scdr s) (force (cdr (force s))))

; -------------------------------------------------------------------

(define (element s k)          ; k-th element of s
  (if (= k 1) 
      (scar s)
      (element (scdr s) (- k 1))))

(define (smerge s1 s2)        ; s1 et s2 infinite ascending streams
  (let ((x1 (scar s1)) (x2 (scar s2)))
    (cond ((< x1 x2) (scons x1 (smerge (scdr s1) s2)))
          ((> x1 x2) (scons x2 (smerge s1 (scdr s2))))
          (else (scons x1 (smerge (scdr s1) (scdr s2)))))))

(define (szoom x S)
  (scons (* x (scar S)) (szoom x (scdr S))))

(define H (scons 1 (smerge (szoom 2 H) (smerge (szoom 3 H) (szoom 5 H)))))   ; Hamming

(time (element H 20000))

;;; SRFI-41 bug

(define (sfrom n step)
  (scons n (sfrom (+ n step) step)))

(define (smap f s)
  (scons (f (scar s)) (smap f (scdr s))))

(define (s->list n s)
  (if (= n 0)
      '()
      (cons (scar s) (s->list (- n 1) (scdr s)))))

(s->list 4 (smap / (sfrom 4 -1)))    ; error ou (1/4 1/3 1/2 1) ?
