#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         (prefix-in r: file/sha1)
         "libcrypto.rkt")

(provide sha256
         sha256-bytes)

(define _SHA256_CTX-pointer _pointer)

(define SHA256_Init 
  (and libcrypto
       (get-ffi-obj 'SHA256_Init libcrypto (_fun _SHA256_CTX-pointer -> _int) (lambda () #f))))
(define SHA256_Update 
  (and libcrypto
       (get-ffi-obj 'SHA256_Update libcrypto (_fun _SHA256_CTX-pointer _pointer _long -> _int) (lambda () #f))))
(define SHA256_Final
  (and libcrypto
       (get-ffi-obj 'SHA256_Final libcrypto (_fun _pointer _SHA256_CTX-pointer -> _int) (lambda () #f))))

(define (sha256-bytes in)
  (unless (input-port? in) (raise-argument-error 'sha256-bytes "input-port?" in))
  (if SHA256_Init
      (let ([ctx (malloc 256)]
            [tmp (make-bytes 4096)]
            [result (make-bytes 32)])
        (SHA256_Init ctx)
        (let loop ()
          (let ([n (read-bytes-avail! tmp in)])
            (unless (eof-object? n)
              (SHA256_Update ctx tmp n)
              (loop))))
        (SHA256_Final result ctx)
        result)
      (lambda () #f)))

(define (sha256 in)
  (unless (input-port? in) (raise-argument-error 'sha256 "input-port?" in))
  (r:bytes->hex-string (sha256-bytes in)))