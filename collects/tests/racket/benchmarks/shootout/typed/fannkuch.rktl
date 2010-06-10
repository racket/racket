;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew
;; Ported to Typed Scheme by Vincent

(require racket/cmdline)

(: fannkuch (Natural -> Natural))
(define (fannkuch n)
  (let ([pi (list->vector 
             (for/list: : (Listof Natural) ([i : Natural (in-range n)]) i))]
        [tmp (make-vector n)]
        [count (make-vector n)])
    (let: loop : Natural
          ([flips : Natural 0]
           [perms : Natural 0]
           [r : Natural n])
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
                (vector-set! count r (assert (sub1 (vector-ref count r)) exact-nonnegative-integer?))
                (cond
                 [(<= (vector-ref count r) 0)
                  (loop2 (add1 r))]
                 [else (loop flips2 (add1 perms) r)]))))))))

(: count-flips ((Vectorof Natural) (Vectorof Natural) -> Natural))
(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let: loop : Natural ([i : Natural 0])
    (if (= (vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (add1 (vector-ref rho 0)))
          (loop (add1 i))))))

(: vector-reverse-slice! (All (X) ((Vectorof X) Natural Natural -> Void)))
(define (vector-reverse-slice! v i j)
  (let: loop : Void
        ([i : Natural i]
         [j : Natural (assert (sub1 j) exact-nonnegative-integer?)])
    (when (> j i)
      (vector-swap! v i j)
      (loop (assert (add1 i) exact-nonnegative-integer?)
            (assert (sub1 j) exact-nonnegative-integer?)))))

(: vector-swap! (All (X) ((Vectorof X) Natural Natural -> Void)))
(define (vector-swap! v i j)
  (let ((t (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(command-line #:args (n)
              (printf "Pfannkuchen(~a) = ~a\n" 
                      n 
                      (fannkuch (assert (string->number (assert n string?)) exact-nonnegative-integer?))))
