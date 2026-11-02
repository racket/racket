#lang racket/base
(require racket/runtime-path
         racket/lazy-require
         ffi/unsafe
         (only-in openssl/private/ffi
                  define-crypto
                  _BIO* _BIO*/null BIO_free
                  _X509* X509_free)
         (only-in openssl/private/verify i2d_X509)
         rackunit)

(lazy-require
 [openssl/private/verify-win32 (win32-verify)]
 [openssl/private/verify-macos (macos-verify)])

(define verify
  (case (system-type 'os*)
    [(windows) win32-verify]
    [(macosx darwin) macos-verify]
    [else void]))

(define-crypto BIO_new_file
  (_fun [path : _path] [mode : _string = "r"] -> _BIO*/null))

(define-crypto PEM_read_bio_X509
  (_fun [bio : _BIO*]
        [_pointer = #f]
        [_pointer = #f]
        [_pointer = #f]
        -> _X509*))

(define (file->der path)
  (define bio (BIO_new_file path))
  (define cert (PEM_read_bio_X509 bio))
  (BIO_free bio)
  (begin0 (i2d_X509 cert)
    (X509_free cert)))

(define-runtime-path dir ".")

;; Test assumes that
;; - root (root_cert.pem) is trusted
;; - subB (subB_cert.pem) is distrusted
;; See setup.txt for instructions.

(define root-cert (file->der (build-path dir "root_cert.pem")))
(define subA-cert (file->der (build-path dir "subA_cert.pem")))
(define subB-cert (file->der (build-path dir "subB_cert.pem")))
(define int1-cert (file->der (build-path dir "int1_cert.pem")))
(define int2-cert (file->der (build-path dir "int2_cert.pem")))
(define int2-byA-cert (file->der (build-path dir "int2_byA_cert.pem")))
(define int3-cert (file->der (build-path dir "int3_cert.pem")))
(define end1-cert (file->der (build-path dir "end1_cert.pem")))
(define end2-cert (file->der (build-path dir "end2_cert.pem")))
(define end3-cert (file->der (build-path dir "end3_cert.pem")))

(define (run-tests)
  (test-case "checking end1 chain (with subB distrusted)"
    (check-equal?
     (verify 'server-auth "server1.racket.example.com" end1-cert (list int1-cert subA-cert))
     #t))
  (test-case "checking end2 chain (with subB distrusted)"
    (check-equal?
     (verify 'server-auth "server2.racket.example.com" end2-cert (list int2-cert subB-cert))
     #f))
  (test-case "checking end2-via-subA chain (with subB distrusted)"
    (check-equal?
     (verify 'server-auth "server2.racket.example.com" end2-cert (list int2-byA-cert subA-cert))
     #t))
  (test-case "checking end3 chain (with subB distrusted)"
    (check-equal?
     (verify 'server-auth "server3.racket.example.com" end3-cert (list int3-cert subB-cert))
     #f)))

;; ----------------------------------------

(module+ main
  (case (system-type 'os)
    [(windows macosx) (run-tests)]
    [else (printf "OS verifier not available on this platform, skipping tests.\n")]))

(module+ test
  (printf "Skipping. This test requires manual setup. Run via main.\n"))
