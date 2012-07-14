#lang typed/racket/base

(require/typed
 racket/base
 [integer-sqrt/remainder (Integer -> (values Integer Integer))])

(provide exact-bits
         exact-integer-sqrt
         exact-pi
         exact-phi)

(: exact-bits (Parameterof Positive-Integer))
(define exact-bits (make-parameter 128))

(: round/bits (Exact-Rational Positive-Integer -> Exact-Rational))
(define (round/bits x bits)
  (define p (arithmetic-shift 1 bits))
  (/ (round (* x p)) p))

(: exact-integer-sqrt (Integer -> Exact-Rational))
;; Returns an approximation of sqrt n, computed using integer-sqrt/remainder + rounding rules
(define (exact-integer-sqrt n)
  (define n-bits (integer-length n))
  ;; Target bits: bits * 2 + 1 ensures the result has at least (exact-bits) bits;
  ;; also add four bits to get two rounding bits from integer-sqrt
  (let* ([bits   (+ 4 (add1 (* 2 (exact-bits))))]
         [shift  (max 0 (- bits n-bits))]
         ;; Make sure dividing resulting exponent by two yields an integer
         [shift  (if (even? shift) shift (add1 shift))])
    (define-values (z z-rem)
      (integer-sqrt/remainder (arithmetic-shift n shift)))
    ;; Adjust least significant rounding bit
    (define rz (if (zero? z-rem) z (bitwise-ior z 1)))
    (* rz (expt 2 (arithmetic-shift (- shift) -1)))))

(: exact-pi (-> Exact-Rational))
;; Returns an approximation of π, computed using Ramanujan 163 with binary splitting
(define (exact-pi)
  (define bits (exact-bits))
  (define N (max 1 (numerator (round (* #e0.05247673 (+ bits 2))))))
  (define A 163096908)
  (define B 6541681608)
  (define J1 10939058860032000)
  (define J3 262537412640768000)
  (define-values (P Q T)
    (let: loop : (Values Integer Integer Integer) ([n1 : Integer  0] [n2 : Integer  N])
      (case (- n2 n1)
        [(1)  (cond [(zero? n1)  (values 1 1 (+ A (* n1 B)))]
                    [else  (define p0 (- (* (- (* 6 n1) 5) (- (* 2 n1) 1) (- (* 6 n1) 1))))
                           (values p0 (* n1 n1 n1 J1) (* (+ A (* n1 B)) p0))])]
        [else  (define m (quotient (+ n1 n2) 2))
               (define-values (Pl Ql Tl) (loop n1 m))
               (define-values (Pr Qr Tr) (loop m n2))
               (values (* Pl Pr) (* Ql Qr)
                       (+ (* Qr Tl) (* Pl Tr)))])))
  (define 1/r (/ Q T))
  ;; Unrounded sqrt J3 works for some reason
  (round/bits (* 1/r (/ (integer-sqrt (arithmetic-shift J3 (* bits 2)))
                        (arithmetic-shift 1 bits)))
              bits))

(: exact-phi (-> Exact-Rational))
;; Returns an approximation of φ, the Golden Ratio
(define (exact-phi)
  ;; Dividing by 2 gives one more bit precision
  (round/bits (* 1/2 (+ 1 (exact-integer-sqrt 5)))
              (exact-bits)))

#;;(: exact-euler (-> Exact-Rational))
(define (exact-euler)
  ...)

#;
(for ([b  (in-range 1 1000)])
  (with-asserts ([b  exact-positive-integer?])
    (define: phi0 : Exact-Rational (parameterize ([exact-bits b]) (exact-phi)))
    (define: phi1 : Exact-Rational (parameterize ([exact-bits (+ 10 (* b 2))])
                                     (round/bits (exact-phi) b)))
    (define e (= phi0 (round/bits phi1 b)))
    (printf "e = ~v~n" e)))
