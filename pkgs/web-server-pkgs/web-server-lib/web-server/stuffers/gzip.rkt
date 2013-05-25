#lang racket/base
(require racket/contract
         web-server/private/gzip
         web-server/stuffers/stuffer)

(define gzip-stuffer
  (make-stuffer gzip/bytes gunzip/bytes))

(provide/contract
 [gzip-stuffer (stuffer/c bytes? bytes?)])
