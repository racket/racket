(module match-tests mzscheme
  (require mzlib/match rackunit)
  (provide match-exn-tests)

  (define simple-fail-tests
    (test-suite
     "Simple fall-through tests"
     (test-case "No clauses"
       (check-exn exn:misc:match?
                  (lambda () (match 3))))
     (test-case "Fall-through with integer"
       (check-exn exn:misc:match?
                  (lambda () (match 3
                               [2 'low]
                               [4 'high]))))
     (test-case "Fall-through with #:when pattern"
       (check-exn exn:misc:match?
                  (lambda () (match 3
                               [x #:when (> x 4) x]))))
     (test-case "Failure procedure"
       (check-exn exn:misc:match?
                  (lambda () (match 3
                               [3 (=> bye)
                                  (bye)
                                  's]
                               [2 ':o]))))))

  (define exn-message-tests-1
    (test-suite
     "Exception messages for match-let-XYZ"
     (test-case "match"
       (check-exn #rx"match: no matching clause for 3"
                  (lambda () (match 3))))
     (test-case "match*"
       (check-exn #rx"match\\*: no matching clause for \\(3 4\\)"
                  (lambda () (match* [3 4]))))
     (test-case "match-let"
       (check-exn #rx"match-let: no matching clause for 3"
                  (lambda () (match-let ([2 3]) ':o))))
     (test-case "match-let-values"
       (check-exn #rx"match-let-values: no matching clause for \\(3 4\\)"
                  (lambda () (match-let-values ([(2 x) (values 3 4)]) ':o))))
     (test-case "match-let*"
       (check-exn #rx"match-let\\*: no matching clause for 3"
                  (lambda () (match-let* ([a 3] [2 a]) ':o))))
     (test-case "match-let*-values"
       (check-exn #rx"match-let\\*-values: no matching clause for \\(3 4\\)"
                  (lambda () (match-let*-values ([(a) 3] [(2 x) (values a 4)]) ':o))))))

  (define exn-message-tests-2
    (test-suite
     "Exception messages other match forms"
     (test-case "match-letrec"
       (check-exn #rx"match-letrec: no matching clause for 3"
                  (lambda () (match-letrec ([a 3] [2 a]) ':o))))
     (test-case "match-letrec-values"
       (check-exn #rx"match-letrec-values: no matching clause for \\(3 5\\)"
                  (lambda () (match-letrec-values ([(a) 3] [(2 _) (values a 5)]) ':o))))
     (test-case "match-define"
       (check-exn #rx"match-define: no matching clause for \\(6 \\. 7\\)"
                  (lambda () (match-define (cons 3 x) '(6 . 7)) x)))
     (test-case "match-define-values"
       (check-exn #rx"match-define-values: no matching clause for \\(6 7\\)"
                  (lambda () (match-define-values (3 x) (values 6 7)) x)))))

  (define match-exn-tests
    (test-suite "Tests for exceptions raised by match.rkt"
                simple-fail-tests
                exn-message-tests-1
                exn-message-tests-2)))
