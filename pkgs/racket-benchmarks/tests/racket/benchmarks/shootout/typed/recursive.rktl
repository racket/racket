;; ---------------------------------------------------------------------
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Code based on / inspired by existing, relevant Shootout submissions
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
;; ---------------------------------------------------------------------

(require scheme/cmdline
         scheme/flonum)

;; -------------------------------

(: ack (Integer Integer -> Integer))
(define (ack m n)
  (cond ((zero? m) (+ n 1))
        ((zero? n) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

;; --------------

(: fib (Integer -> Integer))
(define (fib n)
  (cond ((< n 2) 1)
        (else (+ (fib (- n 2)) (fib (- n 1))))))

(: fibflt (Float -> Float))
(define (fibflt n)
  (cond ((fl< n 2.0) 1.0)
        (else (fl+ (fibflt (fl- n 2.0)) (fibflt (fl- n 1.0))))))

;; --------------

(: tak (Integer Integer Integer -> Integer))
(define (tak x y z)
  (cond ((not (< y x)) z)
        (else (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(: takflt (Float Float Float -> Float))
(define (takflt x y z)
  (cond ((not (fl< y x)) z)
        (else (takflt (takflt (fl- x 1.0) y z) (takflt (fl- y 1.0) z x) (takflt (fl- z 1.0) x y)))))

;; -------------------------------

(: main (Integer -> Void))
(define (main n)

  (printf "Ack(3,~A): ~A\n" n (ack 3 n))
  (printf "Fib(~a): ~a\n" 
          (real->decimal-string (+ 27.0 n) 1)
          (real->decimal-string (fibflt (+ 27.0 n)) 1))
  
  (set! n (- n 1))
  (printf "Tak(~A,~A,~A): ~A\n" (* n 3) (* n 2) n (tak (* n 3) (* n 2) n))
  
  (printf "Fib(3): ~A\n" (fib 3))
  (printf "Tak(3.0,2.0,1.0): ~a\n" (real->decimal-string (takflt 3.0 2.0 1.0) 1)))

;; -------------------------------

(main (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?)))
