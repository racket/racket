
(define (nonnegative-fixnum? n) (and (fixnum? n) (fx>= n 0)))

(define (exact-integer? n) (and (integer? n) (exact? n)))
(define (exact-nonnegative-integer? n) (and (exact-integer? n) (>= n 0)))
(define (exact-positive-integer? n) (and (exact-integer? n) (> n 0)))
(define (inexact-real? n) (and (real? n) (inexact? n)))
(define (byte? n) (and (exact-integer? n) (>= n 0) (<= n 255)))

(define (double-flonum? x) (flonum? x))
(define (single-flonum? x) #f)

(define/who (real->double-flonum x)
  (check who real? x)
  (exact->inexact x))

(define/who (real->single-flonum x)
  (check who real? x)
  (exact->inexact x))

(define arithmetic-shift #2%bitwise-arithmetic-shift)

(define/who (integer-sqrt n)
  (check who integer? n)
  (cond
   [(negative? n) (* (integer-sqrt (- n)) 0+1i)]
   [(positive? n)
    (let-values ([(s r) (exact-integer-sqrt (inexact->exact n))])
      (if (inexact? n)
          (exact->inexact s)
          s))]
   [else n]))

(define/who (integer-sqrt/remainder n)
  (check who integer? n)
  (let ([m (integer-sqrt n)])
    (values m (- n (* m m)))))

(define fx->fl #2%fixnum->flonum)
(define fxrshift #2%fxarithmetic-shift-right)
(define fxlshift #2%fxarithmetic-shift-left)

(define fl->fx #2%flonum->fixnum)
(define ->fl #2%real->flonum)
(define/who (fl->exact-integer fl)
  (check who flonum? fl)
  (inexact->exact (flfloor fl)))

(define/who (flreal-part a)
  (or (and
       (complex? a)
       (not (real? a)) ; => complex imaginary part
       (let ([r (real-part a)])
         (and (flonum? r) r)))
      (check who (lambda (a) #f)
             :contract (string-append
                        "(and/c complex?\n"
                        "       (lambda (c) (flonum? (real-part c)))\n"
                        "       (lambda (c) (flonum? (imag-part c))))")
             a)))

(define/who (flimag-part a)
  (or (and
       (complex? a)
       (let ([r (imag-part a)])
         (and (flonum? r) ; => complex real part
              r)))
      (check who (lambda (a) #f)
             :contract (string-append
                        "(and/c complex?\n"
                        "       (lambda (c) (flonum? (real-part c)))\n"
                        "       (lambda (c) (flonum? (imag-part c))))")
             a)))

(define/who (make-flrectangular a b)
  (check who flonum? a)
  (check who flonum? b)
  (make-rectangular a b))

(define (system-big-endian?)
  (eq? (native-endianness) (endianness big)))

(define/who integer->integer-bytes
  (case-lambda
   [(num size signed? big-endian? bstr start)
    (let ([check (lambda (n lo hi)
                   (check who bytes? bstr)
                   (check who exact-nonnegative-integer? start)
                   (let ([len (bytevector-length bstr)])
                     (unless (>= len n)
                       (raise-arguments-error who
                                              "destination byte string is too small"
                                              "destination byte string length" len
                                              "number of bytes to write" n))
                     (unless (<= start (- len n))
                       (raise-arguments-error who
                                              "starting position too large"
                                              "given starting position" start
                                              "destination byte string length" len
                                              "number of bytes to write" n))
                     (unless (<= lo num hi)
                       (raise-arguments-error who
                                              "number is out of bounds for size in bytes"
                                              "given number" num
                                              (if signed?
                                                  "size in bytes for signed"
                                                  "size in bytes for unsigned")
                                              n))))])
      (case size
        [(1)
         (if signed? 
             (check 1 -128 127)
             (check 1 0 255))
         (if signed?
             (bytevector-s8-set! bstr start num)
             (bytevector-u8-set! bstr start num))]
        [(2)
         (if signed? 
             (check 2 -32768 32767)
             (check 2 0 65535))
         (if signed?
             (bytevector-s16-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little)))
             (bytevector-u16-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little))))]
        [(4)
         (if signed? 
             (check 4 -2147483648 2147483647)
             (check 4 0 8589934591))
         (if signed?
             (bytevector-s32-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little)))
             (bytevector-u32-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little))))]
        [(8)
         (if signed? 
             (check 8 -9223372036854775808 9223372036854775807)
             (check 8 0 18446744073709551615))
         (if signed?
             (bytevector-s64-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little)))
             (bytevector-u64-set! bstr start num (if big-endian?
                                                     (endianness big)
                                                     (endianness little))))]
        [else
         (raise-argument-error 'integer->integer-bytes
                               "(or/c 1 2 4 8)" size)]))
    bstr]
   [(num size signed?)
    (integer->integer-bytes num size signed? (system-big-endian?)
                            (and (exact-integer? size) (<= 1 size 8) (make-bytevector size)) 0)]
   [(num size signed? big-endian?)
    (integer->integer-bytes num size signed? big-endian?
                            (and (exact-integer? size) (<= 1 size 8) (make-bytevector size)) 0)]
   [(num size signed? big-endian? bstr)
    (integer->integer-bytes num size signed? big-endian? bstr 0)]))

(define/who integer-bytes->integer
  (case-lambda
   [(bstr signed? big-endian? start end)
    (check who bytes? bstr)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (case (- end start)
      [(1)
       (if signed?
           (bytevector-s8-ref bstr start)
           (bytevector-u8-ref bstr start))]
      [(2)
       (if signed?
           (bytevector-s16-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u16-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [(4)
       (if signed?
           (bytevector-s32-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u32-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [(8)
       (if signed?
           (bytevector-s64-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u64-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [else
       (raise-arguments-error 'integer-bytes->integer
                              "length is not 1, 2, 4, or 8 bytes"
                              "length" (- end start))])]
   [(bstr signed?)
    (integer-bytes->integer bstr signed? (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr signed? big-endian?)
    (integer-bytes->integer bstr signed? big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr signed? big-endian? start)
    (integer-bytes->integer bstr signed? big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

(define/who real->floating-point-bytes
  (case-lambda
   [(num size big-endian? bstr start)
    (check who bytes? bstr)
    (case size
      [(4)
       (bytevector-ieee-single-set! bstr start num (if big-endian?
                                                       (endianness big)
                                                       (endianness little)))]
      [(8)
       (bytevector-ieee-double-set! bstr start num (if big-endian?
                                                       (endianness big)
                                                       (endianness little)))]
      [else
       (raise-argument-error 'real->floating-point-bytes
                             "(or/c 4 8)" size)])
    bstr]
   [(num size)
    (real->floating-point-bytes num size (system-big-endian?)
                                (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size big-endian?)
    (real->floating-point-bytes num size big-endian?
                                (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size big-endian? bstr)
    (real->floating-point-bytes num size big-endian? bstr 0)]))

(define/who floating-point-bytes->real
  (case-lambda
   [(bstr big-endian? start end)
    (check who bytes? bstr)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (case (- end start)
      [(4)
       (bytevector-ieee-single-ref bstr start (if big-endian?
                                                  (endianness big)
                                                  (endianness little)))]
      [(8)
       (bytevector-ieee-double-ref bstr start (if big-endian?
                                                  (endianness big)
                                                  (endianness little)))]
      [else
       (raise-arguments-error 'floating-point-bytes->real
                              "length is not 4 or 8 bytes"
                              "length" (- end start))])]
   [(bstr)
    (floating-point-bytes->real bstr (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian?)
    (floating-point-bytes->real bstr big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian? start)
    (floating-point-bytes->real bstr big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

(define string->number
  (case-lambda
   [(s) (string->number s 10 #f 'decimal-as-inexact)]
   [(s radix) (string->number s radix #f 'decimal-as-inexact)]
   [(s radix mode) (string->number s radix mode 'decimal-as-inexact)]
   [(s radix mode decimal)
    (if (and (eq? mode 'read) ; => need to watch out for extflonums
             (extflonum-string? s))
        (make-extflonum s)
        ;; The argument is constrained to fixnum, bignum, and flonum forms
        (chez:string->number s radix))]))

(define/who number->string
  (case-lambda
   [(n) (number->string n 10)]
   [(n radix)
    (check who number? n)
    (cond
     [(eq? radix 16)
      ;; Host generates uppercase letters, Racket generates lowercase
      (string-downcase (chez:number->string n 16))]
     [else
      (check who (lambda (radix) (or (eq? radix 2) (eq? radix 8) (eq? radix 10) (eq? radix 16)))
             :contract "(or/c 2 8 10 16)"
             radix)
      (chez:number->string n radix)])]))

(define/who (quotient/remainder n m)
  (check who integer? n)
  (check who integer? m)
  (values (quotient n m) (remainder n m)))

(define/who gcd
  (case-lambda
   [() 0]
   [(n)
    (check who rational? n)
    (abs n)]
   [(n m)
    (check who rational? n)
    (check who rational? m)
    (cond
     [(and (integer? n)
           (integer? m))
      (chez:gcd n m)]
     [else
      (let ([n-n (numerator n)]
            [n-d (denominator n)]
            [m-n (numerator m)]
            [m-d (denominator m)])
        (/ (chez:gcd n-n m-n)
           (chez:lcm n-d m-d)))])]
   [(n . ms)
    (check who rational? n)
    (let loop ([n n] [ms ms])
      (cond
       [(null? ms) n]
       [else (loop (gcd n (car ms)) (cdr ms))]))]))

(define/who lcm
  (case-lambda
   [() 1]
   [(n)
    (check who rational? n)
    (abs n)]
   [(n m)
    (check who rational? n)
    (check who rational? m)
    (cond
     [(and (integer? n)
           (integer? m))
      (chez:lcm n m)]
     [else
      (let ([d (gcd n m)])
        (* n (/ m d)))])]
   [(n . ms)
    (check who rational? n)
    (let loop ([n n] [ms ms])
      (cond
       [(null? ms) n]
       [else (loop (lcm n (car ms)) (cdr ms))]))]))
