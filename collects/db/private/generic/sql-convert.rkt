#lang racket/base

;; ========================================

(provide parse-decimal          ;; used by pg, mysql
         parse-exact-fraction)  ;; used by pg

(define (parse-decimal s)
  (cond [(equal? s "NaN") +nan.0]
        [(regexp-match #rx"^-?([0-9]*)$" s)
         ;; big integer
         => (lambda (m)
              (string->number s))]
        [(regexp-match #rx"^-?([0-9]*)\\.([0-9]*)$" s)
         => (lambda (m)
              (+ (string->number (cadr m))
                 (parse-exact-fraction (caddr m))))]
        [else
         (error 'parse-decimal "internal error: cannot parse ~s as decimal" s)]))

(define (parse-exact-fraction s)
  ;; eg: (parse-exact-fraction "12") = 12/100
  (/ (string->number s)
     (expt 10 (string-length s))))

;; ========================================

(provide marshal-decimal        ;; pg, odbc (?!)
         exact->decimal-string  ;; tests (?)
         exact->scaled-integer) ;; odbc

(define (marshal-decimal f i n)
  (cond [(not (real? n))
         (marshal-error f i "numeric" n)]
        [(eqv? n +nan.0)
         "NaN"]
        [(or (eqv? n +inf.0) (eqv? n -inf.0))
         (marshal-error f i "numeric" n)]
        [(or (integer? n) (inexact? n))
         (number->string n)]
        [(exact? n)
         ;; Bleah.
         (or (exact->decimal-string n)
             (number->string (exact->inexact n)))]))

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

;; exact->scaled-integer : rational -> (cons int nat) or #f
;; Given x, returns (cons M E) s.t. x = (M * 10^-E)
(define (exact->scaled-integer n)
  (let* ([whole-part (truncate n)]
         [fractional-part (- (abs n) (abs whole-part))]
         [den (denominator fractional-part)])
    (let*-values ([(den* fives) (factor-out den 5)]
                  [(den** twos) (factor-out den* 2)])
      (and (= 1 den**)
           (let ([tens (max fives twos)])
             (cons (* n (expt 10 tens)) tens))))))

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

;; ========================================

(provide marshal-error)

;; marshal-error : string datum -> (raises error)
(define (marshal-error f i type datum)
  (error f "cannot marshal as SQL type ~s: ~e"
         type datum))
