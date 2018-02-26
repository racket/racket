#lang racket/base
(provide bytes->char/utf-8)

;; Given a byte and a list of accumulated bytes,
;; return a char, 'fail, or 'continue
(define (bytes->char/utf-8 last-b accum)
  (cond
   [(last-b . < . 128)
    (cond
     [(null? accum) (integer->char last-b)]
     [else 'fail])]
   [(continue-byte? last-b)
    ;; A byte that continues
    (cond
     [(null? accum) 'fail]
     [(two-byte-prefix? (car accum))
      (integer->char*
       #x80
       (+ (arithmetic-shift (bitwise-and #b11111 (car accum)) 6)
          (continue-value last-b)))]
     [(three-byte-prefix? (car accum))
      'continue]
     [(four-byte-prefix? (car accum))
      'continue]
     [(and (pair? (cdr accum))
           (three-byte-prefix? (cadr accum)))
      (integer->char*
       #x800
       (+ (arithmetic-shift (bitwise-and #b1111 (cadr accum)) 12)
          (arithmetic-shift (continue-value (car accum)) 6)
          (continue-value last-b)))]
     [(and (pair? (cdr accum))
           (four-byte-prefix? (cadr accum)))
      'continue]
     [(and (pair? (cdr accum))
           (pair? (cddr accum))
           (four-byte-prefix? (caddr accum)))
      (integer->char*
       #x10000
       (+ (arithmetic-shift (bitwise-and #b1111 (caddr accum)) 18)
          (arithmetic-shift (continue-value (cadr accum)) 12)
          (arithmetic-shift (continue-value (car accum)) 6)
          (continue-value last-b)))]
     [else 'fail])]
   [(and (or (two-byte-prefix? last-b)
             (three-byte-prefix? last-b)
             (four-byte-prefix? last-b))
         (null? accum))
    'continue]
   [else 'fail]))

;; Guard against invalid encodings:
(define (integer->char* lower-bound n)
  (if (or (n . < . lower-bound)
          (n . > . #x10FFFF)
          (and (n . >= . #xD800)
               (n . <= . #xDFFF)))
      'fail
      (integer->char n)))

(define (continue-byte? b)
  (= (bitwise-and b #b11000000) #b10000000))
(define (continue-value b)
  (bitwise-and b #b00111111))

(define (two-byte-prefix? b)
  (= (bitwise-and b #b11100000) #b11000000))
(define (three-byte-prefix? b)
  (= (bitwise-and b #b11110000) #b11100000))
(define (four-byte-prefix? b)
  (= (bitwise-and b #b11111000) #b11110000))
