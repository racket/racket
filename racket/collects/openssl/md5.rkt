#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         (prefix-in r: file/md5)
         "libcrypto.rkt")

(provide md5
         md5-bytes)

(define _SHA_CTX-pointer _pointer)

(define MD5_Init 
  (and libcrypto
       (get-ffi-obj 'MD5_Init libcrypto (_fun _SHA_CTX-pointer -> _int) (lambda () #f))))
(define MD5_Update 
  (and libcrypto
       (get-ffi-obj 'MD5_Update libcrypto (_fun _SHA_CTX-pointer _pointer _long -> _int) (lambda () #f))))
(define MD5_Final
  (and libcrypto
       (get-ffi-obj 'MD5_Final libcrypto (_fun _pointer _SHA_CTX-pointer -> _int) (lambda () #f))))

(define (md5-bytes in)
  (unless (input-port? in) (raise-argument-error 'md5-bytes "input-port?" in))
  (if MD5_Init
      (let ([ctx (malloc 256)]
            [tmp (make-bytes 4096)]
            [result (make-bytes 16)])
        (MD5_Init ctx)
        (let loop ()
          (let ([n (read-bytes-avail! tmp in)])
            (unless (eof-object? n)
              (MD5_Update ctx tmp n)
              (loop))))
        (MD5_Final result ctx)
        result)
      (r:md5 in #f)))

(define (md5 in)
  (unless (input-port? in) (raise-argument-error 'md5 "input-port?" in))
  (bytes->hex-string (md5-bytes in)))

;; copied from `file/sha1` --- should be in a separate module,
;; instead
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
