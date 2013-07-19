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
        (loop (bitwise-and data (vector-ref ones bits)) bits))
      (let ([c (read-byte in)])
        (unless (or (eof-object? c) (eq? c =byte))
          (let ([v (vector-ref base64-digit c)])
            (if v
              (loop (+ (arithmetic-shift data 6) v) (+ bits 6))
              (loop data bits))))))))

(define (base64-encode-stream in out [linesep #"\n"])
  (let loop ([data 0] [bits 0] [width 0])
    (define (write-char)
      (write-byte (vector-ref digit-base64 (arithmetic-shift data (- 6 bits)))
                  out)
      (let ([width (modulo (add1 width) 72)])
        (when (zero? width) (display linesep out))
        width))
    (if (>= bits 6)
      (let ([bits (- bits 6)])
        (loop (bitwise-and data (vector-ref ones bits)) bits (write-char)))
      (let ([c (read-byte in)])
        (if (eof-object? c)
          ;; flush extra bits
          (begin
            (let ([width (if (> bits 0) (write-char) width)])
              (when (> width 0)
                (for ([i (in-range (modulo (- width) 4))])
                  (write-byte =byte out))
                (display linesep out))))
          (loop (+ (arithmetic-shift data 8) c) (+ bits 8) width))))))

(define (base64-decode src)
  (let ([s (open-output-bytes)])
    (base64-decode-stream (open-input-bytes src) s)
    (get-output-bytes s)))

(define (base64-encode src [linesep #"\r\n"])
  (let ([s (open-output-bytes)])
    (base64-encode-stream (open-input-bytes src) s linesep)
    (get-output-bytes s)))
