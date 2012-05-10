#lang racket/base
(require racket/contract
         web-server/stuffers/stuffer
         racket/runtime-path
         openssl/libcrypto
         (rename-in ffi/unsafe
                    [-> f->]))

(define EVP_SHA1
  (and libcrypto
       (get-ffi-obj 'EVP_sha1 libcrypto
                    (_fun f-> _fpointer))))

(define HMAC-SHA1/raw
  (if libcrypto
      (get-ffi-obj 'HMAC libcrypto
                   (_fun [EVP_MD : _fpointer = (EVP_SHA1)]
                         [key : _bytes]
                         [key_len : _int = (bytes-length key)]
                         [data : _bytes]
                         [data_len : _int = (bytes-length data)]
                         [md : (_bytes o 20)]
                         [md_len : (_ptr o _uint)]
                         f-> _bytes
                         f-> md))
      (lambda (key data) (error 'HMAC-SHA1/raw "libcrypto could not load"))))

(define (HMAC-SHA1 key data)
  (HMAC-SHA1/raw key data)
  ; It returns the same pointer always
  #;(bytes-copy
   ; A SHA1 is 20 bytes, including 0s
   (make-sized-byte-string (HMAC-SHA1/raw key data) 20)))

(define (HMAC-SHA1-stuffer key)
  (make-stuffer
   (lambda (ib)
     (bytes-append (HMAC-SHA1 key ib) ib))
   (lambda (ob)
     (define hib (subbytes ob 0 20))
     (define ib (subbytes ob 20))
     (define true-hib (HMAC-SHA1 key ib))
     (if (bytes=? hib true-hib)
         ib
         (error 'HMAC-SHA1-stuffer "Signature does not match!")))))

(provide/contract
 [HMAC-SHA1 (bytes? bytes? . -> . bytes?)]
 [HMAC-SHA1-stuffer (bytes? . -> . (stuffer/c bytes? bytes?))])
