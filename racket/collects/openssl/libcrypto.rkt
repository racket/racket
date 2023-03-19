#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         setup/cross-system
         (for-syntax racket/base
                     setup/cross-system))

(provide libcrypto
         libcrypto-load-fail-reason
         openssl-lib-versions)

;; Notes on shared library versions when provided by OS
;; ie, VERSION s.t. OS provides "lib{crypto,ssl}.{so,dylib}.$VERSION"
;;
;; As of 1.0.0, OpenSSL has adopted a friendlier version policy (see
;; https://www.openssl.org/policies/releasestrat.html), and recent
;; Linux distros (eg Fedora 27, Debian 9.3 ("stretch")) seem to be
;; using (more) compatible .so versions.
;;
;; Notes on older versions:
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
;; - Mac OS includes 0.9.8, 0.9.7, and versionless, but as of 10.15 the
;;   versionless dylib and will abort with the following error:
;;   "Invalid dylib load. Clients should not load the unversioned libcrypto
;;   dylib as it does not have a stable ABI."

(define openssl-lib-versions
  (let
    ([versions
      '("1.1"
        "1.0.2"

        ;; Compatibility-based version / SONAME
        "10"     ;; Fedora
        "1.0.0"  ;; Debian, Ubuntu

        ;; Other specific known versions
        "1.0.1k" "1.0.1j" "1.0.1g" "1.0.1e"
        "1.0" "1.0.0" "1.0.0e" "1.0.0d" "1.0.0c" "1.0.0b" "1.0.0a"
        "0.9.8e" "0.9.8b" "0.9.8" "0.9.7"

        ;; Known versions for *BSD variants
        "111"

        ;; OpenSSL 3 works for most functionality:
        "3")])
    ;; Don't use the versionless dylib on macOS, as it aborts on 10.15
    (case (system-type)
      [(macosx)
       ;; Don't use the versionless dylib on macOS, as it aborts on 10.15.
       ;; Also don't look for older versions, because that can log an error
       ;; in "mzssl.rkt". Just recognize the version that's provided by
       ;; the "racket-lib" package.
       '("1.1")]
      [else
       (case (path->string (system-library-subpath #f))
         [("x86_64-darwin" "i386-darwin" "aarch64-darwin")
          ;; Even in Unix mode, avoid trying versionless on Mac OS
          versions]
         [else
          (cons "" ; versionless (eg from devel pkg)
                versions)])])))

(define libcrypto-load-fail-reason #f)

;; We need to declare because they might be distributed with Racket,
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libcrypto-so
  #:runtime?-id runtime?
  (case (if runtime? (system-type) (cross-system-type))
    [(windows) '(so "libeay32")]
    [(macosx)
     ;; Version "1.1" is bundled with Racket
     '(so "libcrypto" ("1.1" #f))]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (x)
                               (set! libcrypto-load-fail-reason (exn-message x))
                               #f)])
    (ffi-lib libcrypto-so openssl-lib-versions)))
