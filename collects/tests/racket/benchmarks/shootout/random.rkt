;;; http://shootout.alioth.debian.org/
;;; Random implementation, by Jens Axel Sogaard
;;;
;;; Modified for proper string output by Brent Fulgham

#lang racket/base

(require (only-in mzlib/string real->decimal-string))

(define IM 139968)
(define IA   3877)
(define IC  29573)

(define gen_random
  (let ((LAST 42))
    (lambda (max)
      (set! LAST (modulo (+ (* LAST IA) IC) IM))
      (/ (* max LAST) IM))))

(define (roundto digits num)
  (let* ([e (expt 10 digits)]
         [num (round (* e (inexact->exact num)))])
    (format "~a.~a"
            (quotient num e)
            (substring (string-append (number->string (remainder num e))
                                      (make-string digits #\0))
                       0 digits))))

(let ((n (string->number
          (vector-ref (current-command-line-arguments)
                      0))))
  (let loop ((iter n))
    (if (> iter 1)
        (begin
          (gen_random 100.0)
          (loop (- iter 1)))
        #t))
  (printf "~a\n"
          (real->decimal-string (gen_random 100.0) 9)))
