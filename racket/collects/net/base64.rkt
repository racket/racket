#lang racket/base
(require racket/fixnum)

;; Unsafe mode can be worth a factor of 2 to 4
;; on byte-string encoding/decoding:
(#%declare #:unsafe)

(provide base64-encode-stream
         base64-decode-stream
         base64-encode
         base64-decode)

(define ranges '([#"AZ" 0] [#"az" 26] [#"09" 52] [#"++" 62] [#"//" 63]))

(define-values (base64-digit digit-base64)
  (let ([bd (make-vector 256 #f)] [db (make-vector 64 #f)])
    (for ([r ranges] #:when #t
          [i (in-range (bytes-ref (car r) 0) (add1 (bytes-ref (car r) 1)))]
          [n (in-naturals (cadr r))])
      (vector-set! bd i n)
      (vector-set! db n i))
    (values (vector->immutable-vector bd) (vector->immutable-vector db))))

(define (base64-decode-stream in out)
  (unless (input-port? in) (raise-argument-error 'base64-decode-stream "input-port?" in))
  (unless (output-port? out) (raise-argument-error 'base64-decode-stream "output-port?" out))
  (let loop ([data 0] [bits 0])
    (cond
      [(bits . fx>= . 8)
       (let ([bits (fx- bits 8)])
         (write-byte (fxrshift data bits) out)
         (loop (fxand data (fx- (fxlshift 1 bits) 1)) bits))]
      [else
       (define c (read-byte in))
       (unless (or (eof-object? c) (fx= c (char->integer #\=)))
         (let ([v (vector*-ref base64-digit c)])
           (if v
               (loop (fx+ (fxlshift data 6) v) (fx+ bits 6))
               (loop data bits))))])))

(define (base64-encode-stream in out [linesep #"\n"])
  (unless (input-port? in) (raise-argument-error 'base64-encode-stream "input-port?" in))
  (unless (output-port? out) (raise-argument-error 'base64-encode-stream "output-port?" out))
  ;; Each set of three input bytes turns into four output bytes.
  ;; It might be nice to actually write the bytes as a 4-byte string,
  ;; but this way preserves fine-grained streaming.
  (define (o! c) (write-byte (vector*-ref digit-base64 c) out))
  (define (fill!) (write-byte (char->integer #\=) out))
  (define (line!) (display linesep out))
  (let loop ([width 0])
    (define b1 (read-byte in))
    (cond
      [(eof-object? b1)
       (unless (eqv? width 0)
         (line!))]
      [else
       (o! (fxrshift b1 2))
       (define b2 (read-byte in))
       (cond
         [(eof-object? b2)
          (o! (fxlshift (fxand b1 #b11) 4))
          (fill!)
          (fill!)
          (line!)]
         [else
          (o! (fxior (fxlshift (fxand b1 #b11) 4)
                     (fxrshift b2 4)))
          (define b3 (read-byte in))
          (cond
            [(eof-object? b3)
             (o! (fxlshift (fxand b2 #b1111) 2))
             (fill!)
             (line!)]
            [else
             (o! (fxior (fxlshift (fxand b2 #b1111) 2)
                              (fxrshift b3 6)))
             (o! (fxand b3 #b111111))
             (let ([width (if (eqv? width 68)
                              (begin
                                (display linesep out)
                                0)
                              (fx+ width 4))])
               (loop width))])])])))

;; ----------------------------------------

(define (base64-decode src)
  (unless (bytes? src) (raise-argument-error 'base64-decode "bytes?" src))
  ;; Loop through bytes to handle non-encoding characters and stop at `=`
  (define-values (src-len in-len)
    (let loop ([i 0] [len 0])
      (cond
        [(fx= i (bytes-length src)) (values i len)]
        [else
         (define c (bytes-ref src i))
         (cond
           [(fx= c (char->integer #\=)) (values i len)]
           [(vector*-ref base64-digit c) (loop (fx+ i 1) (fx+ len 1))]
           [else (loop (fx+ i 1) len)])])))
  (define out-len (fx+ (fx* (fxrshift in-len 2) 3)
                       (fxmax 0 (fx- (fxand in-len 3) 1))))
  (define out (make-bytes out-len))
  (let loop1 ([i 0] [j 0])
    (unless (fx= i src-len)
      (define c1 (bytes-ref src i))
      (let ([v1 (vector*-ref base64-digit c1)]
            [i (fx+ i 1)])
        (cond
          [(not v1) (loop1 i j)]
          [else
           (let loop2 ([i i] [j j])
             (unless (fx= i src-len)
               (define c2 (bytes-ref src i))
               (let ([v2 (vector*-ref base64-digit c2)]
                     [i (fx+ i 1)])
                 (cond
                   [(not v2) (loop2 i j)]
                   [else
                    (bytes-set! out j (fxior (fxlshift v1 2)
                                             (fxrshift v2 4)))
                    (let loop3 ([i i] [j (fx+ j 1)])
                      (unless (fx= i src-len)
                        (define c3 (bytes-ref src i))
                        (let ([v3 (vector*-ref base64-digit c3)]
                              [i (fx+ i 1)])
                          (cond
                            [(not v3) (loop3 i j)]
                            [else
                             (bytes-set! out j (fxior (fxlshift (fxand v2 #b1111) 4)
                                                      (fxrshift v3 2)))
                             (let loop4 ([i i] [j (fx+ j 1)])
                               (unless (fx= i src-len)
                                 (define c4 (bytes-ref src i))
                                 (let ([v4 (vector*-ref base64-digit c4)]
                                       [i (fx+ i 1)])
                                   (cond
                                     [(not v4) (loop4 i j)]
                                     [else
                                      (bytes-set! out j (fxior (fxlshift (fxand v3 #b11) 6)
                                                               v4))
                                      (loop1 i (fx+ j 1))]))))]))))]))))]))))
  out)

(define (base64-encode src [linesep #"\r\n"])
  (unless (bytes? src) (raise-argument-error 'base64-encode "bytes?" src))
  (cond
    [(and (bytes? src) (bytes? linesep))
     (define in-len (bytes-length src))
     (cond
       [(eqv? 0 in-len) #""]
       [else
        (define out-payload-len (fx* (fxquotient (fx+ in-len 2) 3) 4))
        (define out-len (fx+ out-payload-len
                             (fx* (fxquotient (fx+ out-payload-len 71) 72)
                                  (bytes-length linesep))))
        (define out (make-bytes out-len (char->integer #\=)))
        (define (out! j c) (bytes-set! out j (vector*-ref digit-base64 c)))
        (let loop ([i 0] [j 0] [width 0])
          (cond
            [((fx+ i 3) . fx<= . in-len)
             (define b1 (bytes-ref src i))
             (define b2 (bytes-ref src (fx+ i 1)))
             (define b3 (bytes-ref src (fx+ i 2)))
             (out! j (fxrshift b1 2))
             (out! (fx+ j 1) (fxior (fxlshift (fxand b1 #b11) 4)
                                    (fxrshift b2 4)))
             (out! (fx+ j 2) (fxior (fxlshift (fxand b2 #b1111) 2)
                                    (fxrshift b3 6)))
             (out! (fx+ j 3) (fxand b3 #b111111))
             (let ([width (fx+ width 4)]
                   [i (fx+ i 3)]
                   [j (fx+ j 4)])
               (cond
                 [(and (eqv? width 72)
                       (i . fx< . in-len))
                  (bytes-copy! out j linesep)
                  (loop i (fx+ j (bytes-length linesep)) 0)]
                 [else
                  (loop i j width)]))]
            [((fx+ i 2) . fx<= . in-len)
             (define b1 (bytes-ref src i))
             (define b2 (bytes-ref src (fx+ i 1)))
             (out! j (fxrshift b1 2))
             (out! (fx+ j 1) (fxior (fxlshift (fxand b1 #b11) 4)
                                  (fxrshift b2 4)))
             (out! (fx+ j 2) (fxlshift (fxand b2 #b1111) 2))
             (bytes-copy! out (fx+ j 4) linesep)]
            [((fx+ i 1) . fx<= . in-len)
             (define b1 (bytes-ref src i))
             (out! j (fxrshift b1 2))
             (out! (fx+ j 1) (fxlshift (fxand b1 #b11) 4))
             (bytes-copy! out (fx+ j 4) linesep)]
            [else
             (bytes-copy! out j linesep)]))
        out])]
    [else
     (let ([s (open-output-bytes)])
       (base64-encode-stream (open-input-bytes src) s linesep)
       (get-output-bytes s))]))
