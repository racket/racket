
; Test the oe extension

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(define b1 (class object% () (public [z1 7][z2 8]) (sequence (super-init))))
(define b3 (class object% () (public [z1 13][z2 14]) (sequence (super-init))))

(define i1 (mktinterface (interface () z1)))
(define i3 (mktinterface (interface () z2)))

(define c1 (mktclass b1 i1))
(define c3 (mktclass b3 i3))

(define o1 (make-object c1 1 2))
(define o2 (make-object c1 3 4))
(define o3 (make-object c3 5 6))

(test 5 'oee (send o1 get-y))
(test 5 'oee (send o2 get-y))
(test 5 'oee (send o3 get-y))

(test 7 'oee (send o1 get-z1))
(test 7 'oee (send o2 get-z1))
(test 13 'oee (send o3 get-z1))

(test 8 'oee (send o1 get-z2))
(test 8 'oee (send o2 get-z2))
(test 14 'oee (send o3 get-z2))

(test 1 'oee (send o1 get-x1))
(test 3 'oee (send o2 get-x1))
(test 5 'oee (send o3 get-x1))

(test 2 'oee (send o1 get-x2))
(test 4 'oee (send o2 get-x2))
(test 6 'oee (send o3 get-x2))

(error-test '(mktinterface 0) exn:object:interface-type?)
(error-test '(mktclass 0 i1) exn:object:class-type?)
(error-test '(mktclass b1 0) exn:object:interface-type?)
(error-test '(mktclass b1 (interface () not-there)) exn:object:implement?)

(report-errs)
