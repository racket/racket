#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/convert.rkt"
         "../locale/parameter.rkt")

(provide encoding->bytes
         locale-encoding-is-utf-8?)

;; in atomic mode
(define (encoding->bytes who str)
  (cond
    [(equal? str "")
     (locale-string-encoding/bytes)]
    [else
     (string->bytes/utf-8 str (char->integer #\?))]))
