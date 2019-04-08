;; HACK: hardwired numbers that depend on the tagging regime
;; and other representation details
(define bytevector-content-offset 9)
(define vector-content-offset (if (> (fixnum-width) 32) 9 5))
