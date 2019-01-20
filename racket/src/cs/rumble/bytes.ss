(define/who (bytes . args)
  ;; `bytevector` allows negative numbers that fit in a byte,
  ;; but `bytes` does not
  (for-each (lambda (arg)
              (check who byte? arg))
            args)
  (apply #2%bytevector args))

(define/who (shared-bytes . args)
  (for-each (lambda (arg)
              (check who byte? arg))
            args)
  (apply #2%bytevector args))

(define bytes? #2%bytevector?)

(define bytes-length #2%bytevector-length)

(define/who make-bytes
  (case-lambda
   [(n) (#2%make-bytevector n 0)]
   [(n b)
    (check who exact-nonnegative-integer? n)
    (check who byte? b)
    (unless (and (fixnum? n)
                 (fx<? n 4096))
      (guard-large-allocation who "byte string" n 1))
    (#2%make-bytevector n b)]))

(define/who make-shared-bytes
  (case-lambda
   [(n) (#2%make-bytevector n 0)]
   [(n b)
    (check who exact-nonnegative-integer? n)
    (check who byte? b)
    (#2%make-bytevector n b)]))

(define/who (list->bytes lst)
  (check who
         :test (and (list? lst) (for-each byte? lst))
         :contract "(listof byte?)"
         lst)
  (u8-list->bytevector lst))

(define bytes->list #2%bytevector->u8-list)

(define bytes-ref #2%bytevector-u8-ref)
(define bytes-set! #2%bytevector-u8-set!)
(define bytes->immutable-bytes #2%bytevector->immutable-bytevector)

(define/who bytes-copy!
  (case-lambda
    [(dest d-start src)
     (bytes-copy! dest d-start src 0 (bytes-length src))]
    [(dest d-start src s-start)
     (bytes-copy! dest d-start src s-start (bytes-length src))]
    [(dest d-start src s-start s-end)
     (check who mutable-bytevector? :contract "(and/c bytes? (not/c immutable?))" dest)
     (check who exact-nonnegative-integer? d-start)
     (check who bytes? src)
     (check who exact-nonnegative-integer? s-start)
     (check who exact-nonnegative-integer? s-end)
     (let ([d-len (bytevector-length dest)])
       (check-range who "byte string" dest d-start #f d-len)
       (check-range who "byte string" src s-start s-end (bytevector-length src))
       (let ([s-len (fx- s-end s-start)])
         (check-space who "byte string" d-start d-len s-len)
         (bytevector-copy! src s-start dest d-start s-len)))]))

(define/who (bytes-fill! bstr b)
  (check who mutable-bytevector? :contract "(and/c bytes? (not/c immutable?))" bstr)
  (check who byte? b)
  (bytevector-fill! bstr b))

(define bytes-copy #2%bytevector-copy)

(define-syntax-rule (define-bytes-compare name do-name)
  (define/who name
    (case-lambda
     [(a)
      (check who bytes? a)
      #t]
     [(a b)
      (check who bytes? a)
      (check who bytes? b)
      (do-name a b)]
     [(a b . l)
      (check who bytes? a)
      (check who bytes? b)
      (for-each (lambda (arg)
                  (check who bytes? arg))
                l)
      (and (do-name a b)
           (let loop ([a b] [l l])
             (cond
              [(null? l) #t]
              [else (let ([b (car l)])
                      (and (do-name a b)
                           (loop b (cdr l))))])))])))

(define-bytes-compare bytes=? bytevector=?)

(define (do-bytes<? a b)
  (let ([alen (bytes-length a)]
        [blen (bytes-length b)])
    (let loop ([i 0])
      (cond
       [(= i alen) (if (= i blen)
                       #f
                       #t)]
       [(= i blen) #f]
       [else
        (let ([va (bytes-ref a i)]
              [vb (bytes-ref b i)])
          (cond
           [(fx< va vb) #t]
           [(fx= va vb) (loop (fx1+ i))]
           [else #f]))]))))

(define (do-bytes>? a b)
  (let ([alen (bytes-length a)]
        [blen (bytes-length b)])
    (let loop ([i 0])
      (cond
       [(= i alen) #f]
       [(= i blen) #t]
       [else
        (let ([va (bytes-ref a i)]
              [vb (bytes-ref b i)])
          (cond
           [(fx> va vb) #t]
           [(fx= va vb) (loop (fx1+ i))]
           [else #f]))]))))

(define-bytes-compare bytes<? do-bytes<?)
(define-bytes-compare bytes>? do-bytes>?)

(define/who bytes-append
  (case-lambda 
   [(a b)
    (check who bytes? a)
    (check who bytes? b)
    (let ([alen (bytevector-length a)]
          [blen (bytevector-length b)])
      (let ([c (make-bytevector (+ alen blen))])
        (bytevector-copy! a 0 c 0 alen)
        (bytevector-copy! b 0 c alen blen)
        c))]
   [(a)
    (check who bytes? a)
    (#3%bytevector-copy a)]
   [() #vu8()]
   [args
    (let* ([size (let loop ([args args])
                   (cond
                    [(null? args) 0]
                    [else
                     (let ([arg (car args)])
                       (check who bytes? arg)
                       (+ (bytevector-length arg)
                          (loop (cdr args))))]))]
           [c (make-bytevector size)])
      (let loop ([args args] [pos 0])
        (cond
         [(null? args) c]
         [else
          (let ([len (bytevector-length (car args))])
            (bytevector-copy! (car args) 0 c pos len)
            (loop (cdr args) (+ pos len)))])))]))

(define/who subbytes
  (case-lambda
   [(bstr start end)
    (check who bytes? bstr)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (check-range who "byte string" bstr start end (bytevector-length bstr))
    (let* ([len (- end start)]
           [c (make-bytevector len)])
      (bytevector-copy! bstr start c 0 len)
      c)]
   [(bstr start)
    (subbytes bstr start (bytes-length bstr))]))
