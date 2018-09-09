
(define-record-type (flvector create-flvector flvector?)
  (fields bstr))

(define (flvector=? a b eql?)
  (bytevector=? (flvector-bstr a) (flvector-bstr b)))

(define (flvector-hash-code a hc)
  (hc (flvector-bstr a)))

(define (do-flvector who xs)
  (let ([bstr (make-bytevector (* 8 (length xs)))])
    (let loop ([xs xs] [i 0])
      (unless (null? xs)
        (let ([x (car xs)])
          (check who flonum? x)
          (bytevector-ieee-double-set! bstr i x (native-endianness))
          (loop (cdr xs) (fx+ i 8)))))
    (create-flvector bstr)))

(define new-flvector
  (let ([flvector
         (lambda xs
           (do-flvector 'flvector xs))])
    flvector))

(define (do-make-flvector who size init)
  (check who exact-nonnegative-integer? size)
  (cond
   [(eqv? init 0.0)
    ;; 0-fill bytevector => 0.0-fill flvector
    (create-flvector (make-bytevector (bitwise-arithmetic-shift-left size 3) 0))]
   [else
    (check who flonum? init)
    (let* ([bsize (* 8 size)]
           [bstr (make-bytevector bsize)])
      (let loop ([i 0])
        (unless (= i bsize)
          (bytevector-ieee-double-set! bstr i init (native-endianness))
          (loop (fx+ i 8))))
      (create-flvector bstr))]))

(define make-flvector
  (case-lambda
   [(size) (make-flvector size 0.0)]
   [(size init) (do-make-flvector 'make-flvector size init)]))

(define/who (flvector-length flvec)
  (check who flvector? flvec)
  (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3))

(define (unsafe-flvector-length flvec)
  (#3%fxsrl (#3%bytevector-length (flvector-bstr flvec)) 3))

(define/who (flvector-ref flvec pos)
  (check who flvector? flvec)
  (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
    (check who exact-nonnegative-integer? pos)
    (unless (and (>= pos 0)
                 (< pos len))
      (raise-range-error who "flvector" "" pos flvec 0 len)))
  (bytevector-ieee-double-ref (flvector-bstr flvec)
                              (bitwise-arithmetic-shift-left pos 3)
                              (native-endianness)))

(define (unsafe-flvector-ref flvec pos)
  (#3%bytevector-ieee-double-ref (flvector-bstr flvec)
                                 (#3%fxsll pos 3)
                                 (native-endianness)))

(define/who (flvector-set! flvec pos val)
  (check who flvector? flvec)
  (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
    (check who exact-nonnegative-integer? pos)
    (unless (and (>= pos 0)
                 (< pos len))
      (raise-range-error who "flvector" ""  pos flvec 0 len)))
  (check who flonum? val)
  (bytevector-ieee-double-set! (flvector-bstr flvec)
                               (bitwise-arithmetic-shift-left pos 3)
                               val
                               (native-endianness)))

(define (unsafe-flvector-set! flvec pos val)
  (#3%bytevector-ieee-double-set! (flvector-bstr flvec)
                                  (#3%fxsll pos 3)
                                  val
                                  (native-endianness)))

(define/who flvector-copy
  (case-lambda
   [(flvec) (flvector-copy flvec 0 (flvector-length flvec))]
   [(flvec start) (flvector-copy flvec start (flvector-length flvec))]
   [(flvec start end)
    (check who flvector? flvec)
    (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
      (check who exact-nonnegative-integer? start)
      (check who exact-nonnegative-integer? end)
      (check-range who "flvector" flvec start end len)
      (let* ([new-len (bitwise-arithmetic-shift-left (- end start) 3)]
             [bstr (make-bytevector new-len)])
        (bytes-copy! bstr 0 (flvector-bstr flvec) (bitwise-arithmetic-shift-left start 3) new-len)
        (create-flvector bstr)))]))

(define/who (shared-flvector . xs)
  (register-place-shared (do-flvector who xs)))

(define make-shared-flvector
  (case-lambda
   [(size) (make-shared-flvector size 0.0)]
   [(size init)
    (register-place-shared (do-make-flvector 'make-shared-flvector size init))]))

;; ----------------------------------------

(define (set-flvector-hash!)
  (record-type-equal-procedure (record-type-descriptor flvector)
                               flvector=?)
  (record-type-hash-procedure (record-type-descriptor flvector)
                              flvector-hash-code))
