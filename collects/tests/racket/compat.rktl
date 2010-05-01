

(load-relative "loadtest.rktl")

(Section 'compat)

(require mzlib/compat)

(define-structure (add left right) ([sum (+ left right)]))
(test 9 add-sum (make-add 3 6))


(test #f getprop 'hello-world 'any)
(test 'oops getprop 'hello-world 'any 'oops)
(test #f getprop 'hello-world 'any)
(test (void) putprop 'hello-world 'any 'aha)
(test 'aha getprop 'hello-world 'any)
(test 'aha getprop 'hello-world 'any 'oops)
(test #f getprop 'hello-world 'many)
(test 'oops getprop 'hello-world 'many 'oops)
(test #f getprop 'bye-world 'any)
(test 'oops getprop 'bye-world 'any 'oops)

(report-errs)
