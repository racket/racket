#lang racket/base
(require "special.rkt"
         "delimiter.rkt"
         "accum-string.rkt"
         "error.rkt"
         "consume.rkt"
         "wrap.rkt")

(provide read-delimited-constant)

(define (read-delimited-constant init-c can-match? chars val in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str init-c)
  (let loop ([chars chars])
    (define c (peek-char/special in config))
    (cond
     [(char-delimiter? c config)
      (unless (null? chars)
        (reader-error in config #:due-to c
                      "bad syntax `#~a`" (accum-string-get! accum-str config)))]
     [(null? chars)
      (accum-string-add! accum-str c)
      (reader-error in config
                    "bad syntax `#~a`" (accum-string-get! accum-str config))]
     [(and can-match? (char=? c (car chars)))
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop (cdr chars))]
     [else
      (consume-char/special in config c)
      (accum-string-add! accum-str c)
      (reader-error in config
                    "bad syntax `#~a`" (accum-string-get! accum-str config))]))
  (wrap val in config (accum-string-get! accum-str config)))
