;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew
;; Ported to Typed Scheme by Vincent

(require racket/cmdline)

(: fannkuch (Integer -> Integer))
(define (fannkuch n)
  (let: ([pi : (Vectorof Integer)
             (list->vector
              (for/list: : (Listof Integer) ([i : Integer (in-range n)]) i))]
        [tmp : (Vectorof Integer) (make-vector n)]
        [count : (Vectorof Integer) (make-vector n)])
    (let: loop : Integer
          ([flips : Integer 0]
           [perms : Integer 0]
           [r : Integer n])
      (when (< perms 30)
        (for ([x (in-vector pi)])
          (display (add1 x)))
        (newline))
      (for ([i (in-range r)])
        (vector-set! count i (add1 i)))
      (let ((flips2 (max (count-flips pi tmp) flips)))
        (let loop2 ([r 1])
          (if (= r n)
              flips2
              (let ((perm0 (vector-ref pi 0)))
                (for ([i (in-range r)])
                  (vector-set! pi i (vector-ref pi (add1 i))))
                (vector-set! pi r perm0)
                (vector-set! count r (sub1 (vector-ref count r)))
                (cond
                 [(<= (vector-ref count r) 0)
                  (loop2 (add1 r))]
                 [else (loop flips2 (add1 perms) r)]))))))))

(: count-flips ((Vectorof Integer) (Vectorof Integer) -> Integer))
(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let: loop : Integer ([i : Integer 0])
    (if (= (vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (add1 (vector-ref rho 0)))
          (loop (add1 i))))))

(: vector-reverse-slice! (All (X) ((Vectorof X) Integer Integer -> Void)))
(define (vector-reverse-slice! v i j)
  (let: loop : Void
        ([i : Integer i]
         [j : Integer (sub1 j)])
    (when (> j i)
      (vector-swap! v i j)
      (loop (add1 i) (sub1 j)))))

(: vector-swap! (All (X) ((Vectorof X) Integer Integer -> Void)))
(define (vector-swap! v i j)
  (let ((t (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(command-line #:args (n)
              (printf "Pfannkuchen(~a) = ~a\n" 
                      n 
                      (fannkuch (assert (string->number (assert n string?)) exact-nonnegative-integer?))))
