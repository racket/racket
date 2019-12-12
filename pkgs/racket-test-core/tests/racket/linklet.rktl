
(load-relative "loadtest.rktl")

(Section 'linklet)

(require racket/linklet)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define l (compile-linklet '(linklet
                                  ()
                                  ()
                                4)))
  (test #t linklet? l)
  (test #t instance? (instantiate-linklet l '()))
  (test 4 instantiate-linklet l '() (make-instance 'l))

  (err/rt-test (instantiate-linklet l) exn:fail:contract:arity?)
  (err/rt-test (instantiate-linklet l '#()))
  (err/rt-test (instantiate-linklet l (list 5)))
  (err/rt-test (instantiate-linklet l '() 5)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
