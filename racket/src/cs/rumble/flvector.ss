(define/who make-flvector
  (case-lambda
   [(n) (make-flvector n 0.0)]
   [(n v)
    (unless (and (fixnum? n)
                 (fx< n 1000))
      (guard-large-allocation who 'flvector n (foreign-sizeof 'double)))
    (#2%make-flvector n v)]))

(define/who flvector-copy
  (case-lambda
   [(flvec) (#%flvector-copy flvec)]
   [(flvec start) (flvector-copy flvec start (flvector-length flvec))]
   [(flvec start end)
    (check who flvector? flvec)
    (let ([len (flvector-length flvec)])
      (check who exact-nonnegative-integer? start)
      (check who exact-nonnegative-integer? end)
      (check-range who "flvector" flvec start end len)
      (let* ([new-len (fx- end start)]
             [new-flvec (make-flvector new-len)])
        (let loop ([i 0])
          (unless (fx= i new-len)
            (flvector-set! new-flvec i (flvector-ref flvec (fx+ i start)))
            (loop (fx+ i 1))))
        new-flvec))]))

(define/who (shared-flvector . xs)
  (register-place-shared (apply flvector xs)))

(define make-shared-flvector
  (case-lambda
   [(size) (make-shared-flvector size 0.0)]
   [(size init)
    (register-place-shared (make-flvector size init))]))
