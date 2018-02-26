#lang racket/base
(require "config.rkt"
         "special.rkt"
         "consume.rkt"
         "accum-string.rkt")

(provide read-digits
         digit?
         decimal-digit?
         octal-digit?
         hex-digit?
         digit->number)

(define (read-digits in config [accum-str #f]
                     #:base base #:max-count max-count
                     #:init [init-v 0]
                     #:zero-digits-result [zero-digits-result #f])
  (define c (peek-char/special in config))
  (cond
   [(digit? c base)
    (consume-char in c)
    (when accum-str (accum-string-add! accum-str c))
    (let loop ([v (+ (digit->number c) (* init-v base))]
               [max-count (sub1 max-count)])
      (cond
       [(zero? max-count) v]
       [else
        (define c (peek-char/special in config))
        (cond
         [(digit? c base)
          (consume-char in c)
          (when accum-str (accum-string-add! accum-str c))
          (loop (+ (digit->number c) (* v base)) (sub1 max-count))]
         [else v])]))]
   [zero-digits-result zero-digits-result]
   [else c]))

(define (digit? c base)
  (cond
   [(not (char? c)) #f]
   [(= base 8) (octal-digit? c)]
   [(= base 16) (hex-digit? c)]
   [else (decimal-digit? c)]))

(define (decimal-digit? c)
  (and (char>=? c #\0) (char<=? c #\9)))

(define (octal-digit? c)
  (and (char>=? c #\0) (char<=? c #\7)))

(define (hex-digit? c)
  (or (and (char>=? c #\0) (char<=? c #\9))
      (and (char>=? c #\A) (char<=? c #\F))
      (and (char>=? c #\a) (char<=? c #\f))))

(define (digit->number c)
  (cond
   [(and (char>=? c #\0) (char<=? c #\9))
    (- (char->integer c) (char->integer #\0))]
   [(and (char>=? c #\A) (char<=? c #\F))
    (- (char->integer c) (- (char->integer #\A) 10))]
   [else
    (- (char->integer c) (- (char->integer #\a) 10))]))
