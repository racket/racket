#lang scheme/unit

(require "base64-sig.ss")

(import)
(export base64^)

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

(define (base64-decode-stream in out)
  (let loop ([waiting 0] [waiting-bits 0])
    (if (>= waiting-bits 8)
      (begin
        (write-byte (arithmetic-shift waiting (- 8 waiting-bits)) out)
        (let ([waiting-bits (- waiting-bits 8)])
          (loop (bitwise-and waiting (sub1 (arithmetic-shift 1 waiting-bits)))
                waiting-bits)))
      (let* ([c (read-byte in)]
             [c (if (eof-object? c) =byte c)]
             [v (vector-ref base64-digit c)])
        (cond [v (loop (+ (arithmetic-shift waiting 6) v)
                       (+ waiting-bits 6))]
              [(eq? c =byte) (void)] ; done
              [else (loop waiting waiting-bits)])))))

(define base64-encode-stream
  (case-lambda
    [(in out) (base64-encode-stream in out #"\n")]
    [(in out linesep)
     ;; Process input 3 characters at a time, because 18 bits
     ;;  is divisible by both 6 and 8, and 72 (the line length)
     ;;  is divisible by 3.
     (let ([three (make-bytes 3)]
           [outc (lambda (n)
                   (write-byte (vector-ref digit-base64 n) out))]
           [done (lambda (fill)
                   (for ([i (in-range 0 fill)]) (write-byte =byte out))
                   (display linesep out))])
       (let loop ([pos 0])
         (if (= pos 72)
           ;; Insert newline
           (begin (display linesep out) (loop 0))
           ;; Next group of 3
           (let ([n (read-bytes! three in)])
             (if (eof-object? n)
               (unless (= pos 0) (done 0))
               (let ([a (bytes-ref three 0)]
                     [b (if (n . >= . 2) (bytes-ref three 1) 0)]
                     [c (if (n . >= . 3) (bytes-ref three 2) 0)])
                 (outc (arithmetic-shift a -2))
                 (outc (+ (bitwise-and #x3f (arithmetic-shift a 4))
                          (arithmetic-shift b -4)))
                 (if (n . < . 2)
                   (done 2)
                   (begin (outc (+ (bitwise-and #x3f (arithmetic-shift b 2))
                                   (arithmetic-shift c -6)))
                          (if (n . < . 3)
                            (done 1)
                            (begin (outc (bitwise-and #x3f c))
                                   (loop (+ pos 4))))))))))))]))

(define (base64-decode src)
  (let ([s (open-output-bytes)])
    (base64-decode-stream (open-input-bytes src) s)
    (get-output-bytes s)))

(define (base64-encode src)
  (let ([s (open-output-bytes)])
    (base64-encode-stream (open-input-bytes src) s (bytes 13 10))
    (get-output-bytes s)))
