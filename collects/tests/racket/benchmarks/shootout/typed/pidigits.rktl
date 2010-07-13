;; The Computer Language Shootout
;; http://shootout.alioth.debian.org/
;; Based on the MLton version of the benchmark
;; contributed by Scott Cruzen

(require racket/cmdline)

(: floor_ev (Integer Integer Integer Integer Integer -> Integer))
(define (floor_ev q r s t x)
  (quotient (+ (* q x) r) (+ (* s x) t)))

(: comp (Integer Integer Integer Integer  Integer Integer Integer Integer
                 -> (values Integer Integer Integer Integer)))
(define (comp q r s t  q2 r2 s2 t2)
  (values (+ (* q q2) (* r s2))
          (+ (* q r2) (* r t2))
          (+ (* s q2) (* t s2))
          (+ (* s r2) (* t t2))))

(: next (Integer Integer Integer Integer -> Integer))
(define (next q r s t) (floor_ev q r s t 3))
(: safe? (Integer Integer Integer Integer Integer -> Boolean))
(define (safe? q r s t n) (= n (floor_ev q r s t 4)))
(: prod (Integer Integer Integer Integer Integer
                 -> (values Integer Integer Integer Integer)))
(define (prod q r s t n) (comp 10 (* -10 n) 0 1  q r s t))
(: mk (Integer Integer Integer Integer Integer
               -> (values Integer Integer Integer Integer)))
(define (mk q r s t k) (comp q r s t k (* 2 (add1 (* 2 k))) 0 (add1 (* 2 k))))

(: digit (Integer  Integer Integer Integer Integer  Integer Integer Integer
                   -> Void))
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

(: digits (Integer -> Void))
(define (digits n)
  (digit 1  1 0 0 1  n 0 0))

(digits (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?)))
