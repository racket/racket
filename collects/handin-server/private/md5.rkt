#lang racket/base
(require (prefix-in r: file/md5))
(define (md5 s)
  (bytes->string/latin-1 (r:md5 (string->bytes/utf-8 s))))
(provide md5)
