;; ---------------------------------------------------------------------
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Code based on / inspired by existing, relevant Shootout submissions
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
;; ---------------------------------------------------------------------

(module recursive mzscheme
  (require (only mzlib/string real->decimal-string))

  ;; -------------------------------

  (define (ack m n)
    (cond ((zero? m) (+ n 1))
	  ((zero? n) (ack (- m 1) 1))
	  (else (ack (- m 1) (ack m (- n 1))))))

  ;; --------------

  (define (fib n)
    (cond ((< n 2) 1)
	  (else (+ (fib (- n 2)) (fib (- n 1))))))

  (define (fibflt n)
    (cond ((< n 2.0) 1.0)
	  (else (+ (fibflt (- n 2.0)) (fibflt (- n 1.0))))))

  ;; --------------

  (define (tak x y z)
    (cond ((not (< y x)) z)
	  (else (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

  (define (takflt x y z)
    (cond ((not (< y x)) z)
	  (else (takflt (takflt (- x 1.0) y z) (takflt (- y 1.0) z x) (takflt (- z 1.0) x y)))))

  ;; -------------------------------

  (define (main args)
    (let ((n (string->number (vector-ref args 0))))

      (printf "Ack(3,~A): ~A~%" n (ack 3 n))
      (printf "Fib(~a): ~a~%" 
              (real->decimal-string (+ 27.0 n) 1)
              (real->decimal-string (fibflt (+ 27.0 n)) 1))

      (set! n (- n 1))
      (printf "Tak(~A,~A,~A): ~A~%" (* n 3) (* n 2) n (tak (* n 3) (* n 2) n))

      (printf "Fib(3): ~A~%" (fib 3))
      (printf "Tak(3.0,2.0,1.0): ~a~%" (real->decimal-string (takflt 3.0 2.0 1.0) 1))))

  ;; -------------------------------

  (main (current-command-line-arguments)))
