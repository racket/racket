#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         (prefix-in r: file/sha1))

(provide sha1
         sha1-bytes
         (rename-out [r:bytes->hex-string bytes->hex-string]))

(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (exn) 
                               (log-warning (format "warning: couldn't load OpenSSL library: ~a"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn)))
                               #f)])
    (ffi-lib libcrypto-so '("" "0.9.8b" "0.9.8" "0.9.7"))))

(define _SHA_CTX-pointer _pointer)

(define SHA1_Init 
  (get-ffi-obj 'SHA1_Init libcrypto (_fun _SHA_CTX-pointer -> _int) (lambda () #f)))
(define SHA1_Update 
  (get-ffi-obj 'SHA1_Update libcrypto (_fun _SHA_CTX-pointer _pointer _long -> _int) (lambda () #f)))
(define SHA1_Final
  (get-ffi-obj 'SHA1_Final libcrypto (_fun _pointer _SHA_CTX-pointer -> _int) (lambda () #f)))

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
