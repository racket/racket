#lang racket

(require math/bigfloat
         math/flonum
         math/special-functions
         math/private/bigfloat/bigfloat-hurwitz-zeta)

(collect-garbage)
(collect-garbage)
(collect-garbage)
(define start-mem (current-memory-use))

(define num 60)

(for ([x  (in-range 1 num)])
  (let ([x  (* 0.25 x)])
    (printf "x = ~v~n" x)
    (for ([y  (in-range 1 num)])
      (let ([y  (* 0.25 y)])
        (define z (bigfloat->flonum (bfhurwitz-zeta (bf x) (bf y))))
        (define err (flulp-error (flhurwitz-zeta x y) z))
        (unless (err . <= . 5.0)
          (printf "hurwitz-zeta ~v ~v; error = ~v ulps~n" x y err))))))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(define end-mem (current-memory-use))

(define diff-mem (- end-mem start-mem))
(when (diff-mem . > . (* 2 1024 1024))
  (printf "Significant higher memory use (more than 2MB):~n")
  (printf "   start-mem = ~v~n" start-mem)
  (printf "     end-mem = ~v~n" end-mem)
  (printf "  difference = ~v~n" diff-mem))
