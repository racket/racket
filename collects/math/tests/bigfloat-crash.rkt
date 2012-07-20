#lang racket

(require math/private/mpfr
         ffi/unsafe/atomic)

(define (print-mem)
  (for ([_  (in-range 3)])
    (collect-garbage)
    (printf "memory use: ~v~n" (current-memory-use))))

(printf "0~n")

(print-mem)

(printf "1~n")

(bigfloat->string (bf 1.4e2))

(printf "2~n")

(let ([x  (bf 1)])
  (collect-garbage)
  ;(collect-garbage)
  ;(collect-garbage)
  (for ([_  (in-range 5)])
    (bf 2)
    (time (for ([_  (in-range 20000)])
            #;(bigfloat->rational x)
            (bfexp x)))
    (print-mem)
    ))

(printf "3~n")

(print-mem)
