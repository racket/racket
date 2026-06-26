#lang racket/base
(require racket/path
         ffi/unsafe/runtime-lib
         ffi/unsafe
         rackunit)

;; This test will have to be updated if libraries
;; change as used by the `openssl` package

(define-runtime-lib ssl
  [windows (so "libeay32")
           (so "ssleay32")]
  [(and macosx (or i386 ppc)) (so "libcrypto" ("1.1" #f))
                              (so "libssl" ("1.1" #f))]
  [macosx (so "libcrypto" ("3" #f))
          (so "libssl" ("3" #f))]
  [else 'none])

(define-runtime-lib crypto
  [windows (so "libeay32")]
  [(and macosx (or i386 ppc)) (so "libcrypto" ("1.1" #f))]
  [macosx (so "libcrypto" ("3" #f))]
  [else 'other])

(define-runtime-lib none
  [else 'other])

(define-runtime-lib also-none
  ["no-such-patform"]
  [else 'other])

(check-equal? none 'other)
(check-equal? also-none 'other)

(case (system-type)
  [(windows macosx)
   (check-true (ffi-lib? crypto))
   (check-true (ffi-lib? ssl))]
  [else
   (check-equal? crypto 'other)
   (check-equal? ssl 'none)])

(case (system-type)
  [(windows)
   (check-equal? (normal-case-path (file-name-from-path (ffi-lib-name ssl)))
                 (string->path "ssleay32.dll"))]
  [(macosx)
   (check-equal? (normal-case-path (file-name-from-path (ffi-lib-name ssl)))
                 (case (system-type 'arch)
                   [(i386 pcc) (string->path "libssl.1.dylib")]
                   [else (string->path "libssl.3.dylib")]))]
  [else (void)])

(case (system-type)
  [(windows)
   (check-equal? (normal-case-path (file-name-from-path (ffi-lib-name crypto)))
                 (string->path "libeay32.dll"))]
  [(macosx)
   (check-equal? (normal-case-path (file-name-from-path (ffi-lib-name crypto)))
                 (case (system-type 'arch)
                   [(i386 pcc) (string->path "libcrypto.1.dylib")]
                   [else (string->path "libcrypto.3.dylib")]))]
  [else (void)])

(define runtime-lib-ns (make-base-namespace))
(parameterize ([current-namespace runtime-lib-ns])
  (eval '(require ffi/unsafe/runtime-lib
                  ffi/unsafe)))

(define-syntax-rule (check-syntax e rx:msg)
  (check-exn (lambda (exn)
               (and (exn:fail:syntax? exn)
                    (regexp-match? rx:msg (exn-message exn))))
             (lambda ()
               (parameterize ([current-namespace runtime-lib-ns]
                              [error-print-source-location #f])
                 (eval 'e)))))

(check-syntax (define-runtime-lib x)
              #rx"missing an `else` case")
(check-syntax (define-runtime-lib x
                [()]
                [else #f])
              #rx"expected.*identifier.*string")
(check-syntax (define-runtime-lib x
                #:ffi-lib-args (#f #f)
                [else #f])
              #rx"expected.*keyword")
(check-syntax (define-runtime-lib x
                [nonesuch "oops"]
                [else #f])
              #rx"expected.*`so`")
(check-syntax (define-runtime-lib x
                [nonesuch (so "oops" #f)]
                [else #f])
              #rx"expected library specification")
