#lang scheme/base

(provide bitwise-and
         bitwise-ior
         bitwise-xor
         bitwise-not
         bitwise-if
         (rename-out [integer-length bitwise-length])
         bitwise-first-bit-set
         (rename-out [arithmetic-shift bitwise-arithmetic-shift])
         bitwise-arithmetic-shift-left
         bitwise-arithmetic-shift-right
         bitwise-copy-bit)


(define (bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))

(define (bitwise-first-bit-set b)
  (if (zero? b)
      -1
      (let loop ([b b][pos 0])
        (if (zero? (bitwise-and b 1))
            (loop (arithmetic-shift b -1) (add1 pos))
            pos))))


(define (bitwise-arithmetic-shift-left v s)
  (arithmetic-shift v s))
(define (bitwise-arithmetic-shift-right v s)
  (arithmetic-shift v (- s)))

(define (bitwise-copy-bit a b c)
  (let ([mask (bitwise-arithmetic-shift-left 1 b)])
    (bitwise-if mask
                (bitwise-arithmetic-shift-left c b)
                a)))
