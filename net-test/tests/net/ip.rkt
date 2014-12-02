#lang racket/base

(require net/private/ip tests/eli-tester)

(require (only-in rackunit require/expose))
(require/expose net/private/ip
  (ipv4-string->bytes ipv6-string->bytes ipv4->string ipv6->string
   compress bytes-of-length))

(provide tests)
(module+ main (tests))
(define (tests)
  (test
   ;; ----------------------------------------
   (make-ip-address "127.0.0.1")
   => (ipv4 (bytes 127 0 0 1))
   (make-ip-address (bytes 127 0 0 1))
   => (ipv4 (bytes 127 0 0 1))
   (make-ip-address "2607:f8b0:4009:800::100e")
   => (ipv6 (bytes 38 7 248 176 64 9 8 0 0 0 0 0 0 0 16 14))
   (make-ip-address (bytes 38 7 248 176 64 9 8 0 0 0 0 0 0 0 16 14))
   => (ipv6 (bytes 38 7 248 176 64 9 8 0 0 0 0 0 0 0 16 14))
   (make-ip-address "0.0.0.1")
   => (ipv4 (bytes 0 0 0 1))
   (not (equal? (make-ip-address "128.0.0.1")
                (make-ip-address "255.3.255.0")))
   do (let ([ip-bytes (bytes 127 0 0 1)])
        (define ip (make-ip-address ip-bytes))
        (bytes-set! ip-bytes 0 255)
        (test #:failure-message "IP addresses should be immutable"
              ip => (make-ip-address "127.0.0.1")))
   ;; ----------------------------------------
   (ip-address-string? "0.0.0.0")
   (ip-address-string? "0.1.0.2")
   (ip-address-string? "8.8.8.8")
   (ip-address-string? "12.81.255.109")
   (ip-address-string? "192.168.0.1")
   (ip-address-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
   (ip-address-string? "2001:200:dff:fff1:216:3eff:feb1:44d7")
   (ip-address-string? "2001:db8:85a3:0:0:8a2e:370:7334")
   (ip-address-string? "2001:db8:85a3::8a2e:370:7334")
   (ip-address-string? "0:0:0:0:0:0:0:1")
   (ip-address-string? "0:0:0:0:0:0:0:0")
   (ip-address-string? "::")
   (ip-address-string? "::0")
   (ip-address-string? "::ffff:c000:0280")
   (ip-address-string? "2001:db8::2:1")
   (ip-address-string? "2001:db8:0:0:1::1")
   (not (ip-address-string? ""))
   (not (ip-address-string? ":::"))
   (not (ip-address-string? "::0::"))
   (not (ip-address-string? "2001::db8::2:1"))
   (not (ip-address-string? "2001:::db8:2:1"))
   (not (ip-address-string? "52001:db8::2:1"))
   (not (ip-address-string? "80.8.800.8"))
   (not (ip-address-string? "80.8.800.0"))
   (not (ip-address-string? "080.8.800.8"))
   (not (ip-address-string? "vas8.8.800.8"))
   (not (ip-address-string? "80.8.128.8dd"))
   (not (ip-address-string? "0.8.800.008"))
   (not (ip-address-string? "0.8.800.a8"))
   (not (ip-address-string? "potatoes"))
   (not (ip-address-string? "127.0.0"))
   ;; ----------------------------------------
   (ipv4-string->bytes "0.8.255.0")
   => (bytes 0 8 255 0)
   (ipv4-string->bytes "8.8.8.8")
   => (bytes 8 8 8 8)
   (ipv4-string->bytes "12.81.255.109")
   => (bytes 12 81 255 109)
   (ipv4-string->bytes "192.168.0.1")
   => (bytes 192 168 0 1)
   ;; ----------------------------------------
   (ipv6-string->bytes "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
   => (bytes 32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52)
   (ipv6-string->bytes "2001:200:dff:fff1:216:3eff:feb1:44d7")
   => (bytes 32 1 2 0 13 255 255 241 2 22 62 255 254 177 68 215)
   (ipv6-string->bytes "2001:db8:85a3:0:0:8a2e:370:7334")
   => (bytes 32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52)
   (ipv6-string->bytes "2001:db8:85a3::8a2e:370:7334")
   => (bytes 32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52)
   (ipv6-string->bytes "2607:f8b0:4009:800::100e")
   => (bytes 38 7 248 176 64 9 8 0 0 0 0 0 0 0 16 14)
   (ipv6-string->bytes "::1")
   => (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
   (ipv6-string->bytes "::ffff")
   => (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255)
   ;; ----------------------------------------
   (ip-address->bytes (make-ip-address "8.8.8.8"))
   => (bytes 8 8 8 8)
   (ip-address->bytes (make-ip-address "::1"))
   => (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
   ;; ----------------------------------------
   (ip-address->string (make-ip-address "8.8.8.8"))
   => "8.8.8.8"
   (ip-address->string (make-ip-address "::1"))
   => "::1"
   ;; ----------------------------------------
   (ipv4->string (bytes 0 0 0 0))
   => "0.0.0.0"
   (ipv4->string (bytes 255 255 0 1))
   => "255.255.0.1"
   (ipv4->string (bytes 127 0 0 1))
   => "127.0.0.1"
   (ipv4->string (bytes 8 8 8 8))
   => "8.8.8.8"
   ;; ----------------------------------------
   (ipv6->string (bytes 32 1 13 184 133 163 0 0 0 0 138 46 3 112 115 52))
   => "2001:db8:85a3::8a2e:370:7334"
   (ipv6->string (bytes 38 7 248 176 64 9 8 0 0 0 0 0 0 0 16 14))
   => "2607:f8b0:4009:800::100e"
   (ipv6->string (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255))
   => "::ffff"
   (ipv6->string (bytes 255 255 0 0 0 0 0 0 0 0 0 0 0 0 255 255))
   => "ffff::ffff"
   ;; ----------------------------------------
   (compress '(0 0 0 5 5))   => '(:: 5 5)
   (compress '(0 5 5))       => '(0 5 5)
   (compress '(0 0 5 0 0 5)) => '(:: 5 0 0 5)
   (compress '(0 5 0 0 0 5)) => '(0 5 :: 5)
   ;; ----------------------------------------
   ((bytes-of-length 5) (bytes 1 2 3 4 5))
   (not ((bytes-of-length 5) "moogle"))
   ;; ----------------------------------------
   ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
