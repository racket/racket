#lang racket/base
(require "../host/linklet.rkt")

(provide version-bytes
         vm-bytes)

(define version-bytes (string->bytes/utf-8 (version)))
(define vm-bytes (linklet-virtual-machine-bytes))
