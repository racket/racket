#lang racket/base
(require racket/math)

(provide exact->decimal-string    ;; odbc, tests (?)
         exact->scaled-integer    ;; pg, odbc
         inexact->scaled-integer) ;; pg

;; ========================================

;; exact->decimal-string : exact -> string or #f
;; always includes decimal point
(define (exact->decimal-string n)
  (let* ([whole-part (truncate n)]
         [fractional-part (- (abs n) (abs whole-part))]
         [scaled (exact->scaled-integer fractional-part)])
    (and scaled
         (let* ([ma (car scaled)]
                [ex (cdr scaled)]
                [ma-str (number->string ma)])
           (if (zero? ex)
               (number->string whole-part)
               (string-append (number->string whole-part)
                              "."
                              (make-string (- ex (string-length ma-str)) #\0)
                              ma-str))))))

;; exact->scaled-integer : exact-rational -> (cons int int) or #f
;; Given x, returns (cons M E) s.t. x = (M * 10^-E)
(define (exact->scaled-integer n [trim-integers? #f])
  (if (and trim-integers? (integer? n))
      (let*-values ([(n* fives) (factor-out n 5)]
                    [(n** twos) (factor-out n* 2)])
        (let ([tens (min fives twos)])
          (cons (/ n (expt 10 tens)) (- tens))))
      (let* ([whole-part (truncate n)]
             [fractional-part (- (abs n) (abs whole-part))]
             [den (denominator fractional-part)])
        (let*-values ([(den* fives) (factor-out den 5)]
                      [(den** twos) (factor-out den* 2)])
          (and (= 1 den**)
               (let ([tens (max fives twos)])
                 (cons (* n (expt 10 tens)) tens)))))))

;; inexact->scaled-integer : inexact-rational -> (cons int int)
;; Given x, returns (cons M E) s.t. x ~= (M * 10^-E)
(define (inexact->scaled-integer x)
  ;; FIXME: as a hacky alternative, could just parse result of number->string
  (if (zero? x)
      (cons 0 0)
      ;; nonzero, inexact
      ;; 16 digits ought to be enough (and not too much)
      (let* ([E0 (order-of-magnitude x)]
             ;; x = y * 10^E0 where y in [1,10)
             [E1 (add1 E0)]
             ;; x = y * 10^E1 where y in [0.1,1)
             [E (- E1 16)]
             ;; x ~= M * 10^E where M in [10^15,10^16)
             [M (inexact->exact (truncate (* x (expt 10 (- E)))))]
             ;; trim zeroes from M
             [M*+E* (exact->scaled-integer M #t)]
             [M* (car M*+E*)]
             [E* (cdr M*+E*)])
        (cons M* (- E* E)))))

(define (factor-out-v1 n factor)
  (define (loop n acc)
    (let-values ([(q r) (quotient/remainder n factor)])
      (if (zero? r)
          (loop q (add1 acc))
          (values n acc))))
  (loop n 0))

(define (factor-out n factor)
  (define (loop n factor)
    (if (<= factor n)
        (let*-values ([(q n) (loop n (* factor factor))]
                      [(q* r) (quotient/remainder q factor)])
          (if (zero? r)
              (values q* (+ n n 1))
              (values q  (+ n n))))
        (values n 0)))
  (loop n factor))
