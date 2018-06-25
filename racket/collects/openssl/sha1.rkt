#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         (only-in file/sha1
                  [sha1-bytes r:sha1-bytes]
                  bytes->hex-string
                  hex-string->bytes)
         "libcrypto.rkt")

(provide sha1
         sha1-bytes
         bytes->hex-string
         hex-string->bytes)

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
  (unless (input-port? in) (raise-argument-error 'sha1-bytes "input-port?" in))
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
  (unless (input-port? in) (raise-argument-error 'sha1 "input-port?" in))
  (bytes->hex-string (sha1-bytes in)))
