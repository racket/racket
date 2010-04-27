#lang scheme
(require scheme/serialize
         web-server/stuffers/stuffer
         "../private/util.ss"
         "../private/mod-map.ss")

(define serialize-stuffer
  (make-stuffer
   (lambda (v) (write/bytes (compress-serial (serialize v))))
   (lambda (v) (deserialize (decompress-serial (read/bytes v))))))

(provide/contract
 [serialize-stuffer (stuffer/c serializable? bytes?)])
