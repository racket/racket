#lang scheme

(require scheme/pretty
         srfi/67
         "../require-provide.ss")

(require/provide schemeunit schemeunit/text-ui)

(provide (all-defined-out))

(define-syntax test
  (syntax-rules ()
    [(_ term) (test-case (pretty-format 'term) term)]
    [(_ term ...) (test-case (pretty-format '(begin term ...)) term ...)]))

(define-syntax-rule (test-ok body ...)
  (test (check-ok body ...)))

(define-syntax-rule (test-bad body ...)
  (test (check-bad body ...)))

(define-syntax-rule (with/c c e)
  (let () (with-contract value ([value c]) (define value e)) value))

(define-syntax-rule (check-ok body ...)
  (check-not-exn (lambda () body ...)))

(define-syntax-rule (check-bad body ...)
  (check-exn exn:fail:contract? (lambda () body ...)))

(define-check (check-not compare actual expected)
  (with-check-info*
   (list (make-check-info 'comparison compare)
         (make-check-actual actual)
         (make-check-expected expected))
   (lambda ()
     (let* ([result (compare actual expected)])
       (when result
         (with-check-info*
          (list (make-check-info 'result result))
          (lambda () (fail-check))))))))

(define (check/sort actual expected
                    #:< [<< (<? default-compare)]
                    #:= [== equal?])
  (with-check-info*
   (list (make-check-name 'check/sort)
         (make-check-info '< <<)
         (make-check-info '= ==)
         (make-check-info 'actual actual)
         (make-check-info 'expected expected))
   (lambda ()
     (let* ([actual-sorted (sort actual <<)]
            [actual-length (length actual-sorted)]
            [expected-sorted (sort expected <<)]
            [expected-length (length expected-sorted)])
       (with-check-info*
        (list (make-check-info 'actual-sorted actual-sorted)
              (make-check-info 'expected-sorted expected-sorted))
        (lambda ()
          (unless (= actual-length expected-length)
            (with-check-info*
             (list (make-check-message
                    (format "expected ~a elements, but got ~a"
                            expected-length actual-length)))
             (lambda () (fail-check))))
          (let*-values
              ([(actuals expecteds)
                (for/lists
                    [actuals expecteds]
                    ([actual (in-list actual-sorted)]
                     [expected (in-list actual-sorted)]
                     #:when (not (== actual expected)))
                  (values actual expected))])
            (unless (and (null? actuals) (null? expecteds))
              (with-check-info*
               (list (make-check-info 'actual-failed actuals)
                     (make-check-info 'expected-failed expecteds))
               (lambda () (fail-check)))))))))))
