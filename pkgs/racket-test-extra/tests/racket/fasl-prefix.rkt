#lang racket/base

;; Test that bytestrings created by "the fasl library" start with the name
;;  of the library.
;;
;; In particular:
;; - use `s-exp->fasl` from `racket/fasl` to create a bytestring
;; - read the prefix of the bytestring, i.e. everything before the first ':'
;; - use `dynamic-require` on the prefix to import the `s-exp->fasl` function
;;
;; The idea is that if someone finds a fasl-encoded file, they can view the
;;  file in a text editor and search the internet to learn more --- instead
;;  of being stuck with a mysterious binary.

(require rackunit)

(define SECRET-VALUE 'any-value)

(define bstr
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/fasl)
    (eval `(s-exp->fasl ',SECRET-VALUE))))

(define fasl-prefix
  (string->symbol
    (apply string
           (for/list ([b (in-bytes bstr)]
                       #:break (eq? (integer->char b) #\:))
              (integer->char b)))))

(check-equal?
  ((dynamic-require fasl-prefix 's-exp->fasl) SECRET-VALUE)
  bstr)
