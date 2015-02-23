#lang racket/base

;; The Computer Language Shootout
;; http://shootout.alioth.debian.org/
;; Based on the MLton version of the benchmark
;; contributed by Scott Cruzen

(require racket/cmdline)

(define (floor_ev q r s t x)
  (quotient (+ (* q x) r) (+ (* s x) t)))

(define (comp q r s t  q2 r2 s2 t2)
  (values (+ (* q q2) (* r s2))
          (+ (* q r2) (* r t2))
          (+ (* s q2) (* t s2))
          (+ (* s r2) (* t t2))))

(define (next q r s t) (floor_ev q r s t 3))
(define (safe? q r s t n) (= n (floor_ev q r s t 4)))
(define (prod q r s t n) (comp 10 (* -10 n) 0 1  q r s t))
(define (mk q r s t k) (comp q r s t k (* 2 (add1 (* 2 k))) 0 (add1 (* 2 k))))

(define (digit k  q r s t  n row col)
  (if (> n 0)
      (let ([y (next q r s t)])
        (if (safe? q r s t y)
            (let-values ([(q r s t) (prod q r s t y)])
              (if (= col 10)
                  (let ([row (+ row 10)])
                    (printf "\t:~a\n~a" row y)
                    (digit k q r s t (sub1 n) row 1))
                  (begin
                    (printf "~a" y)
                    (digit k q r s t (sub1 n) row (add1 col)))))
            (let-values ([(q r s t) (mk q r s t k)])
              (digit (add1 k) q r s t n row col))))
      (printf "~a\t:~a\n"
              (make-string (- 10 col) #\space)
              (+ row col))))

(define (digits n)
  (digit 1  1 0 0 1  n 0 0))

(digits (command-line #:args (n) (string->number n)))
