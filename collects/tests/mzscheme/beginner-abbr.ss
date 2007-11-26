
;; Basic checks for the beginner language. Error messages really
;; should be inspected manually.

;; Limitations of this test suite:
;;  - It doesn't check reader-level parameterization, such as use of quotes
;;  - It doesn't check format of printed results
;;  - It doesn't check the absence of MzScheme forms

;; Don't try to run other tests from the test suite after loading this
;; one into a particular namespace.

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

(require (only-in mzscheme 
                  exn:fail?
                  exn:fail:contract?))

(define current-htdp-lang 'lang/htdp-beginner-abbr)
(load-relative "htdp-test.ss")

(require (lib "htdp-beginner-abbr.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intml.ss")
(load-relative "beg-intm.ss")
(load-relative "beg-bega.ss")
(load-relative "bega-adv.ss")

(report-errs)
