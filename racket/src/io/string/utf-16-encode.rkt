#lang racket/base
(require racket/fixnum
         "../common/set-two.rkt")

(provide utf-16-encode)

(define (utf-16-encode s)
  (define surrogate-count
    (for/fold ([n 0]) ([c (in-string s)])
      (if ((char->integer c) . fx>= . #x10000)
          (fx+ n 1)
          n)))
  (define bstr (make-bytes (fx* 2 (fx+ (string-length s) surrogate-count))))
  (for/fold ([pos 0]) ([c (in-string s)])
    (define v (char->integer c))
    (cond
      [(v . fx>= . #x10000)
       (define av (fx- v #x10000))
       (define hi (fxior #xD800 (fxand (fxrshift av 10) #x3FF)))
       (define lo (fxior #xDC00 (fxand av #x3FF)))
       (bytes-set-two! bstr pos (fxrshift hi 8) (fxand hi #xFF))
       (bytes-set-two! bstr pos (fxrshift lo 8) (fxand lo #xFF))
       (fx+ pos 4)]
      [else
       (bytes-set-two! bstr pos (fxrshift v 8) (fxand v #xFF))
       (fx+ pos 2)]))
  bstr)
