#lang racket/base
(require racket/fixnum
         "../common/set-two.rkt")

(provide utf-16-decode)

(define big-endian? (system-big-endian?))

(define (utf-16-decode bstr)
  (define len (bytes-length bstr))
  (define surrogate-count
    (if (fx= len 0)
        0
        (for/fold ([n 0]) ([b (in-bytes bstr (if big-endian? 0 1) len 2)])
          (if (fx= (fxand b #xDC) #xD8)
              (fx+ n 1)
              n))))
  (define str (make-string (fx- (fxrshift len 1) surrogate-count)))
  (let loop ([i 0] [pos 0])
    (unless (fx= i len)
      (define a (bytes-ref bstr i))
      (define b (bytes-ref bstr (fx+ i 1)))
      (define v (if big-endian?
                    (fxior (fxlshift a 8) b)
                    (fxior (fxlshift b 8) a)))
      (cond
        [(fx= (fxand v #xDC00) #xDC00)
         ;; surrogate pair
         (define a (bytes-ref bstr (fx+ i 2)))
         (define b (bytes-ref bstr (fx+ i 3)))
         (define v2 (if big-endian?
                        (fxior (fxlshift a 8) b)
                        (fxior (fxlshift b 8) a)))
         (define all-v (fx+ #x10000
                            (fxior (fxlshift (fxand v #x3FF) 10)
                                   (fxand v2 #x3FF))))
         (string-set! str pos (integer->char all-v))
         (loop (fx+ i 4) (fx+ pos 1))]
        [else
         (string-set! str pos (integer->char v))
         (loop (fx+ i 2) (fx+ pos 1))])))
  str)
