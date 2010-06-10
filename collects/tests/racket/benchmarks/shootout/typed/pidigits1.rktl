; The Computer Language Shootout
; http://shootout.alioth.debian.org/
; Sven Hartrumpf 2005-04-12
; Implements 'Spigot' algorithm origionally due to Stanly Rabinowitz.
; This program is based on an implementation for SCM by Aubrey Jaffer and
; Jerry D. Hedden.

(: pi (Natural Natural -> Void))
(define (pi n d)
  (let*: ((r : Natural (assert (inexact->exact (floor (exp (* d (log 10))))) exact-nonnegative-integer?)) ; 10^d
          (p : Natural (+ (quotient n d) 1))
          (m : Natural (quotient (* p d 3322) 1000))
          (a : (Vectorof Natural) (make-vector (+ m 1) 2))
          (out : Output-Port (current-output-port)))
    (vector-set! a m 4)
    (let: j-loop : Void
          ([b : Natural 2][digits : Natural 0])
      (if (= digits n)
          ;; Add whitespace for ungenerated digits
          (let ([left (modulo digits 10)])
            (unless (zero? left)
              (fprintf out "~a\t:~a\n" (make-string (assert (- 10 left) exact-nonnegative-integer?) #\space) n)))
          ;; Compute more digits
          (let loop ([k m][q 0])
            (if (zero? k)
                (let* ((s (let ([s (number->string (+ b (quotient q r)))])
                            (if (zero? digits)
                                s
                                (string-append (make-string (assert (- d (string-length s)) exact-nonnegative-integer?) #\0) s)))))
                  (j-loop (remainder q r)
                          (print-digits out s 0 (string-length s) digits n)))
                (let ([q (+ q (* (vector-ref a k) r))])
                  (let ((t (+ (* k 2) 1)))
                    (let-values ([(qt rr) (quotient/remainder q t)])
                      (vector-set! a k rr)
                      (loop (sub1 k) (* k qt)))))))))))

(: print-digits (Output-Port String Natural Natural Natural Natural -> Natural))
(define (print-digits out s start end digits n)
  (let*: ([len : Natural (assert (- end start) exact-nonnegative-integer?)]
          [cnt : Natural (assert (min len (- n digits) (- 10 (modulo digits 10)) len) exact-nonnegative-integer?)])
    (if (zero? cnt)
        digits
        (begin
          (write-string s out start (+ start cnt))
          (let ([digits (+ digits cnt)])
            (when (zero? (modulo digits 10))
              (fprintf out "\t:~a\n" digits))
            (print-digits out s (+ start cnt) end digits n))))))

(: main ((Vectorof String) -> Void))
(define (main args)
  (let ((n (if (= (vector-length args) 0)
               1
               (assert (string->number (vector-ref args 0)) exact-nonnegative-integer?))))
    (pi n 10)))

(main (current-command-line-arguments))
