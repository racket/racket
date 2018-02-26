#lang racket/base
(require "../common/set-two.rkt")

(provide utf-16-decode)

(define big-endian? (system-big-endian?))

(define (utf-16-decode bstr)
  (define len (bytes-length bstr))
  (define surrogate-count
    (for/fold ([n 0]) ([b (in-bytes bstr (if big-endian? 0 1) len 2)])
      (if (= (bitwise-and b #xDC) #xD8)
          (add1 n)
          n)))
  (define str (make-string (- (arithmetic-shift len -1) surrogate-count)))
  (let loop ([i 0] [pos 0])
    (unless (= i len)
      (define a (bytes-ref bstr i))
      (define b (bytes-ref bstr (add1 i)))
      (define v (if big-endian?
                    (bitwise-ior (arithmetic-shift a 8) b)
                    (bitwise-ior (arithmetic-shift b 8) a)))
      (cond
        [(= (bitwise-and v #xDC00) #xDC00)
         ;; surrogate pair
         (define a (bytes-ref bstr (+ i 2)))
         (define b (bytes-ref bstr (+ i 3)))
         (define v2 (if big-endian?
                        (bitwise-ior (arithmetic-shift a 8) b)
                        (bitwise-ior (arithmetic-shift b 8) a)))
         (define all-v (+ #x10000
                          (bitwise-ior (arithmetic-shift (bitwise-and v #x3FF) 10)
                                       (bitwise-and v2 #x3FF))))
         (string-set! str pos (integer->char all-v))
         (loop (+ i 4) (add1 pos))]
        [else
         (string-set! str pos (integer->char v))
         (loop (+ i 2) (add1 pos))])))
  str)
