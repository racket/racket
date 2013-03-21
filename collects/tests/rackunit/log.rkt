#lang racket/base
(require rackunit
         rackunit/log)

(define-syntax-rule (&& label stdout-e stdout-p)
  (let ()
    (define stdout-ev stdout-e)
    (define stdout-av stdout-p)
    (unless (equal? stdout-ev stdout-av)
      (error 'log "Bad ~a: ~v vs ~v" label stdout-ev stdout-av))))

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

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "" "" 0)
(& (test-log #:exit? #t) "" "" 0)
(& (test-log #:display? #t #:exit? #t) "" "" 0)

(check-true #t)

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "1 test passed\n" "" 0)
(& (test-log #:exit? #t) "" "" 0)
(& (test-log #:display? #t #:exit? #t) "1 test passed\n" "" 0)

(parameterize ([current-error-port (current-output-port)])
  (check-true #f))

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "" "1/2 test failures\n" 0)
(& (test-log #:exit? #t) "" "" 1)
(& (test-log #:display? #t #:exit? #t) "" "1/2 test failures\n" 1)
