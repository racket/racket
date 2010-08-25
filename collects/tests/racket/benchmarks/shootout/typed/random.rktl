;;; http://shootout.alioth.debian.org/
;;; Random implementation, by Jens Axel Sogaard
;;;
;;; Modified for proper string output by Brent Fulgham
;;; Modified for Typed Scheme by Vincent St-Amour

(require (only-in mzlib/string real->decimal-string))

(define IM 139968)
(define IA   3877)
(define IC  29573)

(define gen_random
  (let: ((LAST : Integer 42))
    (lambda: ((max : Float))
      (set! LAST (modulo (+ (* LAST IA) IC) IM))
      (/ (* max LAST) IM))))

(: roundto (Natural Float -> String))
(define (roundto digits num)
  (let*: ([e : Integer (expt 10 digits)]
          [num : Integer (round (* e (inexact->exact num)))])
    (format "~a.~a"
            (quotient num e)
            (substring (string-append (number->string (remainder num e))
                                      (make-string digits #\0))
                       0 digits))))

(let ((n (assert (string->number
                  (vector-ref (current-command-line-arguments)
                              0)) exact-integer?)))
  (let loop ((iter n))
    (if (> iter 1)
        (begin
          (gen_random 100.0)
          (loop (- iter 1)))
        #t))
  (printf "~a\n"
          (real->decimal-string (gen_random 100.0) 9)))
