
(load-relative "loadtest.rktl")

(Section 'foreign-inline)

;; ------------------------------------------------------------

;; Expressions in `#%foreign-inline` depend on the linklet-level
;; compiler, but we can expect usual Scheme things to be there

(test #t (#%foreign-inline #t))
(test #t procedure? (#%foreign-inline (lambda (x) x)))
(test #t procedure? (#%foreign-inline (lambda (x) x) #:copy))
(test #t procedure? (#%foreign-inline (lambda (x) x) #:pure))
(test #t procedure? (#%foreign-inline (lambda (x) x) #:effect))

(syntax-test #'#%foreign-inline)
(syntax-test #'(#%foreign-inline))
(syntax-test #'(#%foreign-inline 1 2))
(syntax-test #'(#%foreign-inline #:pure #t))
(syntax-test #'(#%foreign-inline #t #:wrong))

(when (eq? 'chez-scheme (system-type 'vm))
  (eval '(test #t procedure? (#%foreign-inline (foreign-procedure 0 (int int) int))))
  (eval '(test 7 (#%foreign-inline (meta-cond [#true 7] [else #f])))))

;; ------------------------------------------------------------

(report-errs)
