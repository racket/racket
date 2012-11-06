#lang racket/base

(require file/sha1 rackunit)


;; The docs say that sha1 must return a 40-character string,
;; and should include leading zeros.
(check-equal? (string-length (sha1 (open-input-string " r a c k et")))
              40)
