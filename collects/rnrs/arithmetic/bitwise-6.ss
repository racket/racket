#lang scheme/base

(provide bitwise-and
         bitwise-ior
         bitwise-xor
         bitwise-not
         bitwise-if
         (rename-out [integer-length bitwise-length])
         bitwise-first-bit-set
         bitwise-bit-set?
         (rename-out [arithmetic-shift bitwise-arithmetic-shift])
         bitwise-arithmetic-shift-left
         bitwise-arithmetic-shift-right
         bitwise-copy-bit)


(define (bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))

(define (bitwise-bit-count i)
  (if (negative? i)
      (bitwise-not (bitwise-bit-count (bitwise-not i)))
      (let loop ([i i][cnt 0])
        (if (zero? i)
            cnt
            (loop (arithmetic-shift i -1)
                  (+ cnt (if (eq? 1 (bitwise-and i 1)) 1 0)))))))

(define (bitwise-first-bit-set b)
  (if (zero? b)
      -1
      (let loop ([b b][pos 0])
        (if (zero? (bitwise-and b 1))
            (loop (arithmetic-shift b -1) (add1 pos))
            pos))))

(define (bitwise-bit-set? b n)
  (eq? 1 (bitwise-and (arithmetic-shift b (- n)) 1)))

(define (bitwise-copy-bit b n bit)
  (unless (or (eq? bit 1)
              (eq? bit 0))
    (raise-type-error 'bitwise-copy-bit "0 or 1" bit))
  (if (eq? bit 1)
      (bitwise-ior b (arithmetic-shift 1 n))
      (bitwise-xor b (arithmetic-shift 1 n))))

(define (bitwise-bit-field b start end)
  (bitwise-and (arithmetic-shift b (- start))
               (sub1 (arithmetic-shift 1 (- end start)))))

(define (bitwise-copy-bit-field to start end from)
  (let* ([mask1 (arithmetic-shift -1 start)]
         [mask2 (bitwise-not (arithmetic-shift -1 end))]
         [mask (bitwise-and mask1 mask2)])
    (bitwise-if mask
                (arithmetic-shift from start)
                to)))

(define (bitwise-arithmetic-shift-left v s)
  (arithmetic-shift v s))
(define (bitwise-arithmetic-shift-right v s)
  (arithmetic-shift v (- s)))

