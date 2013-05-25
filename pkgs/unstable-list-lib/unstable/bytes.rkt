#lang racket/base
(require racket/contract/base)

(provide/contract
 [read/bytes (bytes? . -> . printable/c)]
 [write/bytes (printable/c . -> . bytes?)])

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
