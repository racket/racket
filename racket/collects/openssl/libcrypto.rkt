#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(provide libcrypto
         libcrypto-load-fail-reason
         openssl-lib-versions)

;; Notes on shared library versions when provided by OS
;; ie, VERSION s.t. OS provides "lib{crypto,ssl}.{so,dylib}.$VERSION"
;;
;; - Debian and Ubuntu use a few fixed library versions even though
;;   actual OpenSSL version changes:
;;   - Debian squeeze: lib{crypto,ssl}.so.0.9.8
;;   - Debian {wheezy, jessie, stretch, sid}: lib{crypto,ssl}.so.1.0.0
;;   - Ubuntu {14.04, 14.10, 15.04}: lib{crypto,ssl}.so.1.0.0
;;   - Debian and Ubuntu also provide versionless library in pkg "libssl-dev"
;; - Fedora provides libraries suffixed with actual versions (eg
;;   1.0.1k) as well as a simply-versioned symlink (eg libssl.so.10):
;;   - Fedora {19, 20}: lib{crypto,ssl}.so.1.0.1e, also lib{crypto,ssl}.so.10
;;   - Fedora 21: lib{crypto,ssl}.so.1.0.1j, also lib{crypto,ssl}.so.10
;;   - Fedora 22: lib{crypto,ssl}.so.1.0.1k, also lib{crypto,ssl}.so.10
;;   - Fedora also provides a versionless library in pkg "openssl-devel"
;; - Mac OS X includes 0.9.8, 0.9.7, and versionless

(define openssl-lib-versions
  '(;; Versionless (eg from devel pkg)
    ""

    ;; Compatibility-based version / SONAME
    "10"     ;; Fedora
    "1.0.0"  ;; Debian, Ubuntu

    ;; Other specific known versions
    "1.0.1k" "1.0.1j" "1.0.1g" "1.0.1e"
    "1.0" "1.0.0" "1.0.0e" "1.0.0d" "1.0.0c" "1.0.0b" "1.0.0a"
    "0.9.8e" "0.9.8b" "0.9.8" "0.9.7"))

(define libcrypto-load-fail-reason #f)

;; We need to declare because they might be distributed with Racket,
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (x)
                               (set! libcrypto-load-fail-reason (exn-message x))
                               #f)])
    (ffi-lib libcrypto-so openssl-lib-versions)))
