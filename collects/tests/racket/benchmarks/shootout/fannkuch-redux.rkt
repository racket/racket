#lang racket/base

(require racket/unsafe/ops)

;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew

(require racket/cmdline)

(define (fannkuch n)
  (let ([pi (list->vector 
             (for/list ([i (in-range n)]) i))]
        [tmp (make-vector n)]
        [count (make-vector n)])
    (let loop ([flips 0]
               [perms 0]
               [r n]
               [checksum 0]
               [even-parity? #t])
      (for ([i (in-range r)])
        (unsafe-vector-set! count i (unsafe-fx+ 1 i)))
      (let* ((next-flips (count-flips pi tmp))
             (flips2 (max next-flips flips))
             (next-checksum (unsafe-fx+ checksum (if even-parity? next-flips (unsafe-fx- 0 next-flips)))))
        (let loop2 ([r 1])
          (if (unsafe-fx= r n)
              (values flips2 next-checksum)
              (let ((perm0 (unsafe-vector-ref pi 0)))
                (for ([i (in-range r)])
                  (unsafe-vector-set! pi i (unsafe-vector-ref pi (unsafe-fx+ 1 i))))
                (unsafe-vector-set! pi r perm0)
                (unsafe-vector-set! count r (unsafe-fx- (unsafe-vector-ref count r) 1))
                (cond
                  [(<= (unsafe-vector-ref count r) 0)
                   (loop2 (unsafe-fx+ 1 r))]
                  [else (loop flips2 
                              (unsafe-fx+ 1 perms)
                              r 
                              next-checksum
                              (not even-parity?))]))))))))

(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let loop ([i 0])
    (if (unsafe-fx= (unsafe-vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (unsafe-fx+ 1 (unsafe-vector-ref rho 0)))
          (loop (unsafe-fx+ 1 i))))))

(define (vector-reverse-slice! v i j)
  (let loop ([i i]
             [j (unsafe-fx- j 1)])
    (when (unsafe-fx> j i)
      (vector-swap! v i j)
      (loop (unsafe-fx+ 1 i) (unsafe-fx- j 1)))))

(define-syntax-rule (vector-swap! v i j)
  (let ((t (unsafe-vector-ref v i)))
    (unsafe-vector-set! v i (unsafe-vector-ref v j))
    (unsafe-vector-set! v j t)))

(command-line #:args (n)
              (define-values (answer checksum)
                (fannkuch (string->number n)))
              (printf "~a\nPfannkuchen(~a) = ~a\n" 
                      checksum
                      n 
                      answer))
