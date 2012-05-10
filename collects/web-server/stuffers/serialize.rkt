#lang racket/base
(require racket/contract
         racket/serialize
         web-server/stuffers/stuffer
         web-server/private/util
         web-server/private/mod-map)

(define serialize-stuffer
  (make-stuffer
   (lambda (v) (write/bytes (compress-serial (serialize v))))
   (lambda (v) (deserialize (decompress-serial (read/bytes v))))))

(provide/contract
 [serialize-stuffer (stuffer/c serializable? bytes?)])
