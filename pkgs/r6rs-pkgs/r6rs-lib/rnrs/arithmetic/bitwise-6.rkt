#lang scheme/base

(provide bitwise-and
         bitwise-ior
         bitwise-xor
         bitwise-not
         bitwise-if
         (rename-out [integer-length bitwise-length])
         bitwise-bit-count
         bitwise-first-bit-set
         bitwise-bit-set?
         bitwise-copy-bit
         bitwise-bit-field
         (rename-out [arithmetic-shift bitwise-arithmetic-shift])
         bitwise-arithmetic-shift-left
         bitwise-arithmetic-shift-right
         bitwise-copy-bit-field
         bitwise-rotate-bit-field
         bitwise-reverse-bit-field)


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

(define (bitwise-copy-bit b n bit)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'bitwise-copy-bit "exact nonnegative integer" n))
  (unless (or (eq? bit 1)
              (eq? bit 0))
    (raise-type-error 'bitwise-copy-bit "0 or 1" bit))
  (if (eq? bit 1)
      (bitwise-ior b (arithmetic-shift 1 n))
      (bitwise-and b (bitwise-not (arithmetic-shift 1 n)))))

(define (bitwise-copy-bit-field to start end from)
  (unless (exact-nonnegative-integer? start)
    (raise-type-error 'bitwise-copy-bit-field "exact nonnegative integer" start))
  (unless (exact-nonnegative-integer? end)
    (raise-type-error 'bitwise-copy-bit-field "exact nonnegative integer" end))
  (unless (start . <= . end)
    (error 'bitwise-copy-bit-field "ending position ~e is not as big a starting position ~e" start end))
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

(define (bitwise-rotate-bit-field n start end count)
  (unless (exact-nonnegative-integer? start)
    (raise-type-error 'bitwise-rotate-bit-field "exact nonnegative integer" start))
  (unless (exact-nonnegative-integer? end)
    (raise-type-error 'bitwise-rotate-bit-field "exact nonnegative integer" end))
  (unless (start . <= . end)
    (error 'bitwise-rotate-bit-field "ending position ~e is not as big a starting position ~e" start end))
  (unless (exact-nonnegative-integer? count)
    (raise-type-error 'bitwise-rotate-bit-field "exact nonnegative integer" count))
  (let* ([width (- end start)]
         [count (modulo count width)]
         [field0 (bitwise-bit-field n start end)]
         [field1 (arithmetic-shift field0 count)]
         [field2 (arithmetic-shift field0 (- count width))]
         [field (bitwise-ior field1 field2)])
    (bitwise-copy-bit-field n start end field)))

(define (bitwise-reverse-bit-field n start end)
  (unless (exact-nonnegative-integer? start)
    (raise-type-error 'bitwise-rotate-bit-field "exact nonnegative integer" start))
  (unless (exact-nonnegative-integer? end)
    (raise-type-error 'bitwise-rotate-bit-field "exact nonnegative integer" end))
  (unless (start . <= . end)
    (error 'bitwise-rotate-bit-field "ending position ~e is not as big a starting position ~e" start end))
  (let ([field (bitwise-bit-field n start end)]
        [width (- end start)])
    (let loop ([old field][new 0][width width])
      (cond
       [(zero? width) (bitwise-copy-bit-field n start end new)]
       [else (loop (arithmetic-shift old -1)
                   (bitwise-ior (arithmetic-shift new 1)
                                (bitwise-and old 1))
                   (sub1 width))]))))
