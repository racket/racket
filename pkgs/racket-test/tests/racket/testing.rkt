#lang racket/base
(require rackunit
         raco/testing)

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))

(define-syntax-rule (&& label stdout-e stdout-p)
  (let ()
    (define stdout-ev stdout-e)
    (define stdout-av stdout-p)
    (unless (equal? stdout-ev stdout-av)
      (error 'log "bad ~a\n actual: ~v\n expected: ~v"
             label stdout-av stdout-ev))))

;; TODO(jackfirth): This testing system is pretty weird, and it's only necessary
;;   because the test log is a pair of global variables. That makes it hard to
;;   keep these tests from affecting the test log used by raco test. If the test
;;   log were a parameter, then these tests could simply create a new log and
;;   parameterize the current log to it. We should probably do that instead.
(define-syntax-rule (& test-e stdout-e stderr-e exit-e)
  (let ()
    (define stdout-p (open-output-string))
    (define stderr-p (open-output-string))
    (define exit-av 0)
    (parameterize ([current-output-port stdout-p]
                   [current-error-port stderr-p]
                   [exit-handler (λ (ec) (set! exit-av ec))])
      test-e)
    (&& 'stdout stdout-e (get-output-string stdout-p))
    (&& 'stderr stderr-e (get-output-string stderr-p))
    (&& 'exit-code exit-e exit-av)))

(& (test-report) "" "" 0)
(& (test-report #:display? #t) "" "" 0)
(& (test-report #:exit? #t) "" "" 0)
(& (test-report #:display? #t #:exit? #t) "" "" 0)

(check-true #t)

(& (test-report) "" "" 0)
(& (test-report #:display? #t) "1 test passed\n" "" 0)
(& (test-report #:exit? #t) "" "" 0)
(& (test-report #:display? #t #:exit? #t) "1 test passed\n" "" 0)

(parameterize ([current-error-port (open-output-string)])
  (check-true #f))

(& (test-report) "" "" 0)
(& (test-report #:display? #t) "" "1/2 test failures\n" 0)
(& (test-report #:exit? #t) "" "" 1)
(& (test-report #:display? #t #:exit? #t) "" "1/2 test failures\n" 1)

(parameterize ([test-log-enabled? #f])
  (check-true #t)
  (& (test-report) "" "" 0)
  (& (test-report #:display? #t) "" "1/2 test failures\n" 0)
  (& (test-report #:exit? #t) "" "" 1)
  (& (test-report #:display? #t #:exit? #t) "" "1/2 test failures\n" 1))

(test-begin
 (check-true #t))

(& (test-report #:display? #t) "" "1/3 test failures\n" 0)

(parameterize ([current-error-port (open-output-string)])
  (test-begin
   (check-true #f)))

(& (test-report #:display? #t) "" "2/4 test failures\n" 0)
