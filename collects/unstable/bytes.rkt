#lang racket/base
(require racket/contract
         racket/serialize)

(provide/contract
 [read/bytes (bytes? . -> . serializable?)]
 [write/bytes (serializable? . -> . bytes?)]
 [bytes-ci=? (bytes? bytes? . -> . boolean?)])

(define (bytes-ci=? b0 b1)
  (string-ci=? (bytes->string/utf-8 b0)
               (bytes->string/utf-8 b1)))
;; Eli: If this ever gets in, it should say that the memory requirements
;;   are 4 times the input size, especially since bytes are often used to save
;;   space.  Also, fails on (bytes-ci=? #"\277" #"\277"), and a trivial fix
;;   would still fail on (bytes-ci=? #"\276\277" #"\277\276")

(define (read/bytes bs)
  (read (open-input-bytes bs)))
;; Eli: This is a really bad name for something that is often called
;;   `read-from-string', or `read-from-bytes' in this case.  I first read it as
;;   "read with bytes".  Regardless, I see little point in composing two
;;   functions where the two names are clear enough -- you might consider
;;   looking at the version in CL.
;; Ryan: I agree. More useful would be a version that checked that the
;; bytes contains only one S-expr and errors otherwise.

(define (write/bytes v)
  (define by (open-output-bytes))
  (write v by)
  (get-output-bytes by))
;; Eli: Same bad name as above.  Also, is there any point in this given
;;   (format "~s" v), and the fact that using the resulting string for printout
;;   will get the same result.
