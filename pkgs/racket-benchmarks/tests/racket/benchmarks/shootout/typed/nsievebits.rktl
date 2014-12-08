;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;
;; Adapted from CMUCL code by Dima Dorfman; bit-vector stuff by Alex Shinn;
;; cobbled together by felix, converted to MzScheme by Brent Fulgham
;; converted to Typed Scheme by Vincent St-Amour

(require scheme/cmdline)

(: make-bit-vector (Integer -> Bytes))
(define (make-bit-vector size)
  (let* ((len (quotient (+ size 7) 8))
         (res (make-bytes len #b11111111)))
    (let ((off (remainder size 8)))
      (unless (zero? off)
        (bytes-set! res (- len 1) (- (arithmetic-shift 1 off) 1))))
    res))

(: bit-vector-ref (Bytes Integer -> Boolean))
(define (bit-vector-ref vec i)
  (let ((byte (arithmetic-shift i -3))
        (off (bitwise-and i #x7)))
    (and (< byte (bytes-length vec))
         (not (zero? (bitwise-and (bytes-ref vec byte)
                                  (arithmetic-shift 1 off)))))))

(: bit-vector-set! (Bytes Integer Boolean -> Void))
(define (bit-vector-set! vec i x)
  (let ((byte (arithmetic-shift i -3))
        (off (bitwise-and i #x7)))
    (let ((val (bytes-ref vec byte))
          (mask (arithmetic-shift 1 off)))
      (bytes-set! vec
                  byte
                  (if x
                      (bitwise-ior val mask)
                      (bitwise-and val (bitwise-not mask)))))))

(: nsievebits (Integer -> Integer))
(define (nsievebits m)
  (let ((a (make-bit-vector m)))
    (: clear (Integer -> Void))
    (define (clear i)
      (do: : Void
           ([j : Integer (+ i i) (+ j i)])
           ((>= j m))
        (bit-vector-set! a j #f)))
    (let: ([c : Integer 0])
      (do ([i 2 (add1 i)])
          ((>= i m) c)
        (when (bit-vector-ref a i)
          (clear i)
          (set! c (add1 c)))))))

(: string-pad (String Integer -> String))
(define (string-pad s len)
  (string-append (make-string (- len (string-length s)) #\space)
                 s))

(: test (Natural -> Void))
(define (test n)
  (let* ((m (* (expt 2 n) 10000))
         (count (nsievebits m)))
    (printf "Primes up to ~a ~a\n"
            (string-pad (number->string m) 8)
            (string-pad (number->string count) 8))))

(: main (Integer -> Void))
(define (main n)
  (when (>= n 0) (test n))
  (when (>= n 1) (test (assert (- n 1) exact-nonnegative-integer?)))
  (when (>= n 2) (test (assert (- n 2) exact-nonnegative-integer?))))

(command-line #:args (n) (main (assert (string->number (assert n string?)) exact-nonnegative-integer?)))
