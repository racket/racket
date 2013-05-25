#lang racket
(define (formats v u)
  (if (equal? v -inf.0)
      "ε"
      (with-handlers ([exn:fail? (lambda (x)
                                   (format "~a~a"
                                           (number->string v) u))])
        (format "~a~a"
                (real->decimal-string v 2) u))))

(define (format-duration-h h)
  (formats h "h"))

(define (format-duration-m m)
  (if (m . >= . 60)
      (format-duration-h (/ m 60))
      (formats m "m")))

(define (format-duration-s s)
  (if (s . >= . 60)
      (format-duration-m (/ s 60))
      (formats s "s")))

(define (format-duration-ms ms)
  (if (ms . >= . 1000)
      (format-duration-s (/ ms 1000))
      (formats ms "ms")))

(provide/contract
 [formats (number? string? . -> . string?)]
 [format-duration-h (number? . -> . string?)]
 [format-duration-m (number? . -> . string?)]
 [format-duration-s (number? . -> . string?)]
 [format-duration-ms (number? . -> . string?)])
