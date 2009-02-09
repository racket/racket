#lang scheme
(require web-server/stuffers/stuffer
         scheme/runtime-path
         (rename-in scheme/foreign
                    [-> f->]))

(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(unsafe!)

(define libcrypto
  (ffi-lib libcrypto-so '("" "0.9.8b" "0.9.8" "0.9.7")))

(define EVP_SHA1
  (get-ffi-obj 'EVP_sha1 libcrypto-so
               (_fun f-> _fpointer)))

(define HMAC-SHA1/raw
  (get-ffi-obj 'HMAC libcrypto-so
               (_fun [EVP_MD : _fpointer = (EVP_SHA1)]
                     [key : _bytes]
                     [key_len : _int = (bytes-length key)]
                     [data : _bytes]
                     [data_len : _int = (bytes-length data)]
                     [md : _int = 0]
                     [md_len : _int = 0]
                     f->
                     _pointer)))

(define (HMAC-SHA1 key data)
  ; It returns the same pointer always
  (bytes-copy
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