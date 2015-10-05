#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         (for-syntax racket/base)
         (prefix-in r: file/sha1)
         "libcrypto.rkt")

(provide sha256
         sha256-bytes)

(define-ffi-definer define-libcrypto libcrypto)

;https://fossies.org/dox/openssl-1.0.2d/include_2openssl_2sha_8h.html#a8902af97bc4411166213b43c6d2057d2
(define SHA_LBLOCK 16)

;https://fossies.org/dox/openssl-1.0.2d/include_2openssl_2sha_8h.html#a1b2e699d6af9a09d35e23c231e415c6c
(define _SHA_LONG _uint)

; https://fossies.org/dox/openssl-1.0.2d/structSHA256state__st.html
; typedef SHA256_CTX SHA256state_st;
(define-cstruct _SHA256_CTX ([h (_array/list _SHA_LONG 8)]
                             [NI _SHA_LONG]
                             [Nh _SHA_LONG]
                             [data (_array/list _SHA_LONG SHA_LBLOCK)]
                             [num _uint]
                             [md_len _uint]))

(define-libcrypto SHA256_Init
  (_fun (ctx : (_ptr o _SHA256_CTX))
        -> _int
        -> (cast ctx _pointer _SHA256_CTX-pointer)))

(define-libcrypto SHA256_Update
  (_fun _SHA256_CTX-pointer _bytes _long -> _int))

(define-libcrypto SHA256_Final
  (_fun (result : (_bytes o 32))
        _SHA256_CTX-pointer
        -> _int
        -> result))

; Input-Port -> Bytes
; generates the message digest of the contents of the input port, returning it as bytestring.
(define (sha256-bytes in)
  (unless (input-port? in) (raise-argument-error 'sha256-bytes "input-port?" in))
  (if SHA256_Init
      (let ([ctx (SHA256_Init)]
            [tmp (make-bytes 4096)])
        (let loop ()
          (let ([n (read-bytes-avail! tmp in)])
            (unless (eof-object? n)
              (SHA256_Update ctx tmp n)
              (loop))))
        (SHA256_Final ctx))
      (raise (make-exn:fail:unsupported
              "OpenSSL SHA-256 implementation not found"
              (current-continuation-marks)))))

; Input-Port -> String
; generates the message digest of the contents of the input port, returning it as hex-string.
(define (sha256 in)
  (unless (input-port? in) (raise-argument-error 'sha256 "input-port?" in))
  (r:bytes->hex-string (sha256-bytes in)))
