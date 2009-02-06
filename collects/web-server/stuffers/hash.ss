#lang scheme
(require web-server/stuffers/stuffer
         web-server/stuffers/store
         file/md5)

(define hash/c
  (bytes? . -> . bytes?))

(define (hash-stuffer hash store)
  (make-stuffer
   (lambda (v)
     (define hv (hash v))
     ((store-write store) hv v)
     hv)
   (lambda (hv)
     ((store-read store) hv))))

(define (md5-stuffer home)
  (hash-stuffer md5 (dir-store home)))

(provide/contract
 [hash/c contract?]
 [hash-stuffer (hash/c store? . -> . (stuffer/c bytes? bytes?))]
 [md5-stuffer (path-string? . -> . (stuffer/c bytes? bytes?))])