#lang racket/base
(require "../common/set-two.rkt")

(provide utf-16-encode)

(define (utf-16-encode s)
  (define surrogate-count
    (for/fold ([n 0]) ([c (in-string s)])
      (if ((char->integer c) . >= . #x10000)
          (add1 n)
          n)))
  (define bstr (make-bytes (* 2 (+ (string-length s) surrogate-count))))
  (for/fold ([pos 0]) ([c (in-string s)])
    (define v (char->integer c))
    (cond
      [(v . >= . #x10000)
       (define av (- v #x10000))
       (define hi (bitwise-ior #xD800 (bitwise-and (arithmetic-shift av -10) #x3FF)))
       (define lo (bitwise-ior #xDC00 (bitwise-and av #x3FF)))
       (bytes-set-two! bstr pos (arithmetic-shift hi -8) (bitwise-and hi #xFF))
       (bytes-set-two! bstr pos (arithmetic-shift lo -8) (bitwise-and lo #xFF))
       (+ pos 4)]
      [else
       (bytes-set-two! bstr pos (arithmetic-shift v -8) (bitwise-and v #xFF))
       (+ pos 2)]))
  bstr)
