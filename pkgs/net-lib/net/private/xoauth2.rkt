#lang racket/base
(require net/base64)

(provide xoauth2-encode)

(define (xoauth2-encode username password)
  (base64-encode
   (string->bytes/utf-8
    (string-append "user=" username "\x01auth=Bearer " password "\x01\x01"))
   #""))
