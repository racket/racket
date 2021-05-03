#lang racket/base

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

(define =byte (bytes-ref #"=" 0))
(define ones
  (vector->immutable-vector
   (list->vector (for/list ([i (in-range 9)]) (sub1 (arithmetic-shift 1 i))))))

(define (base64-decode-stream in out)
  (let loop ([data 0] [bits 0])
    (if (>= bits 8)
      (let ([bits (- bits 8)])
        (write-byte (arithmetic-shift data (- bits)) out)
        (loop (bitwise-and data (vector*-ref ones bits)) bits))
      (let ([c (read-byte in)])
        (unless (or (eof-object? c) (eq? c =byte))
          (let ([v (vector*-ref base64-digit c)])
            (if v
              (loop (+ (arithmetic-shift data 6) v) (+ bits 6))
              (loop data bits))))))))

(define (base64-encode-stream in out [linesep #"\n"])
  ;; each set of three input bytes turns into four output bytes
  (define (o! c) (write-byte (vector*-ref digit-base64 c) out))
  (define (fill!) (write-byte (char->integer #\=) out))
  (define (line!) (display linesep out))
  (let loop ([width 0])
    (define b1 (read-byte in))
    (unless (eof-object? b1)
      (let ([width (if (eqv? width 72)
                       (begin
                         (display linesep out)
                         0)
                       width)])
        (o! (arithmetic-shift b1 -2))
        (define b2 (read-byte in))
        (cond
          [(eof-object? b2)
           (o! (arithmetic-shift (bitwise-and b1 #b11) 4))
           (fill!)
           (fill!)
           (line!)]
          [else
           (o! (bitwise-ior (arithmetic-shift (bitwise-and b1 #b11) 4)
                            (arithmetic-shift b2 -4)))
           (define b3 (read-byte in))
           (cond
             [(eof-object? b3)
              (o! (arithmetic-shift (bitwise-and b2 #b1111) 2))
              (fill!)
              (line!)]
             [else
              (o! (bitwise-ior (arithmetic-shift (bitwise-and b2 #b1111) 2)
                               (arithmetic-shift b3 -6)))
              (o! (bitwise-and b3 #b111111))
              (loop (+ width 4))])])))))

(define (base64-decode src)
  (let ([s (open-output-bytes)])
    (base64-decode-stream (open-input-bytes src) s)
    (get-output-bytes s)))

(define (base64-encode src [linesep #"\r\n"])
  (let ([s (open-output-bytes)])
    (base64-encode-stream (open-input-bytes src) s linesep)
    (get-output-bytes s)))
