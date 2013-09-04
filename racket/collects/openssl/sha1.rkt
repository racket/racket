#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         (prefix-in r: file/sha1)
         "libcrypto.rkt")

(provide sha1
         sha1-bytes
         (rename-out [r:bytes->hex-string bytes->hex-string])
         (rename-out [r:hex-string->bytes hex-string->bytes]))

(define _SHA_CTX-pointer _pointer)

(define SHA1_Init 
  (and libcrypto
       (get-ffi-obj 'SHA1_Init libcrypto (_fun _SHA_CTX-pointer -> _int) (lambda () #f))))
(define SHA1_Update 
  (and libcrypto
       (get-ffi-obj 'SHA1_Update libcrypto (_fun _SHA_CTX-pointer _pointer _long -> _int) (lambda () #f))))
(define SHA1_Final
  (and libcrypto
       (get-ffi-obj 'SHA1_Final libcrypto (_fun _pointer _SHA_CTX-pointer -> _int) (lambda () #f))))

(define (sha1-bytes in)
  (if SHA1_Init
      (let ([ctx (malloc 256)]
            [tmp (make-bytes 4096)]
            [result (make-bytes 20)])
        (SHA1_Init ctx)
        (let loop ()
          (let ([n (read-bytes-avail! tmp in)])
            (unless (eof-object? n)
              (SHA1_Update ctx tmp n)
              (loop))))
        (SHA1_Final result ctx)
        result)
      (r:sha1-bytes in)))

(define (sha1 in)
  (r:bytes->hex-string (sha1-bytes in)))
