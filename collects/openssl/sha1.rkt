#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(provide sha1
         sha1-bytes
         bytes->hex-string)

(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (ffi-lib libcrypto-so '("" "0.9.8b" "0.9.8" "0.9.7")))

(define _SHA_CTX-pointer _pointer)

(define SHA1_Init 
  (get-ffi-obj 'SHA1_Init libcrypto (_fun _SHA_CTX-pointer -> _int)))
(define SHA1_Update 
  (get-ffi-obj 'SHA1_Update libcrypto (_fun _SHA_CTX-pointer _pointer _long -> _int)))
(define SHA1_Final
  (get-ffi-obj 'SHA1_Final libcrypto (_fun _pointer _SHA_CTX-pointer -> _int)))

(define (sha1-bytes in)
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
    result))

(define (sha1 in)
  (bytes->hex-string (sha1-bytes in)))

(define (bytes->hex-string bstr)
  (let* ([len (bytes-length bstr)]
         [bstr2 (make-bytes (* len 2))]
         [digit
          (lambda (v)
            (if (v . < . 10)
                (+ v (char->integer #\0))
                (+ v (- (char->integer #\a) 10))))])
    (for ([i (in-range len)])
      (let ([c (bytes-ref bstr i)])
        (bytes-set! bstr2 (* 2 i) (digit (arithmetic-shift c -4)))
        (bytes-set! bstr2 (+ (* 2 i) 1) (digit (bitwise-and c #xF)))))
    (bytes->string/latin-1 bstr2)))
