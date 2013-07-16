#lang racket/base
(require rackunit)
(require macro-debugger/model/debug
         macro-debugger/model/stx-util
         "gentest-framework.rkt"
         "test-setup.rkt")
(provide mk-deriv-test
         mk-steps-test
         mk-hidden-deriv-test
         mk-hidden-steps-test)

(define (mk-deriv-test protos)
  (mk-test "Derivations" checker-for-deriv protos))

(define (mk-steps-test protos)
  (mk-test "Reductions" checker-for-steps protos))

(define (mk-hidden-deriv-test protos)
  (mk-test "Hiding: Completes for multiple policies"
           checker-for-hidden-deriv protos))

(define (mk-hidden-steps-test protos)
  (mk-test "Hiding: Reductions" checker-for-hidden-steps protos))

(define (mk-test label checker protos)
  (make-test-suite label
                   (filter values 
                           (map (mk-gen-test checker) protos))))

(define (mk-gen-test f)
  (define (gen prototest)
    (match prototest
      [(struct collection (label contents))
       (let ([tests (filter values (map gen contents))])
         (and (pair? tests)
              (make-test-suite label tests)))]
      [(struct individual (label form attrs))
       (f label form attrs)]))
  gen)

(define (checker-for-deriv label form attrs)
  (cond [(assq '#:ok-deriv? attrs)
         => (lambda (key+expect-ok?)
              (delay-test
               (test-case label
                 (let ([d (trace/ns form (assq '#:kernel attrs))])
                   (check-pred deriv? d)
                   (if (cdr key+expect-ok?)
                       (check-pred ok-node? d)
                       (check-pred interrupted-node? d))))))]
        [else #f]))

(define (checker-for-hidden-deriv label form attrs)
  (cond [(assq '#:ok-deriv? attrs)
         => (lambda (key+expect-ok?)
              (delay-test
               (test-case label
                 (let ([d (trace/ns form (assq '#:kernel attrs))]
                       [expect-ok? (cdr key+expect-ok?)])
                   (check-hide d hide-none-policy expect-ok?)
                   (check-hide d hide-all-policy expect-ok?)
                   (check-hide d T-policy expect-ok?)))))]
        [else #f]))

(define (check-hide d policy expect-ok?)
  (let-values ([(steps binders uses stx2 exn)
                (parameterize ((macro-policy policy))
                  (reductions+ d))])
    (check-pred list? steps)
    (check-pred reduction-sequence? steps)
    (check-true (not (and stx2 exn)) "Must not produce both estx and exn")
    (if expect-ok?
        (check-pred syntax? stx2 "Expected expanded syntax")
        (check-pred exn? exn "Expected syntax error exn"))))

(define (checker-for-steps label form attrs)
  (cond [(assq '#:steps attrs)
         => (lambda (key+expected)
              (delay-test
               (test-case label
                 (let* ([d (trace/ns form (assq '#:kernel attrs))]
                        [rs (reductions d)])
                   (check-steps (cdr key+expected) rs)))))]
        [else #f]))

(define (checker-for-hidden-steps label form attrs)
  (cond [(assq '#:same-hidden-steps attrs)
         (unless (assq '#:steps attrs)
           (error 'checker-for-hidden-steps "no steps given for ~s" label))
         (delay-test
          (test-case label
            (let* ([d (trace/ns form (assq '#:kernel attrs))]
                   [rs (parameterize ((macro-policy T-policy))
                         (reductions d))])
              (check-steps (cdr (assq '#:steps attrs)) rs))))]
        [(assq '#:hidden-steps attrs)
         => (lambda (key+expected)
              (delay-test
               (test-case label
                 (let* ([d (trace/ns form (assq '#:kernel attrs))]
                        [rs (parameterize ((macro-policy T-policy))
                              (reductions d))])
                   (check-steps (cdr (assq '#:hidden-steps attrs)) rs)))))]
        [else #f]))

(define (check-steps expected actual)
  (check-pred list? actual)
  (check-pred reduction-sequence? actual)
  (with-check-info (;;['actual-sequence-raw actual]
                    ['actual-sequence
                     (for/list ([thing actual])
                       (cond [(misstep? thing)
                              'error]
                             [(remarkstep? thing)
                              (list* 'remark
                                     (protostep-type thing)
                                     (map syntax->datum (filter syntax? (remarkstep-contents thing))))]
                             [else
                              (list* (protostep-type thing)
                                     (syntax->datum (step-term2 thing))
                                     (map syntax->datum
                                          (map bigframe-term (state-lctx (protostep-s1 thing)))))]))]
                    ['expected-sequence expected])
    (compare-step-sequences actual expected)))

(define (reduction-sequence? rs)
  (andmap protostep? rs))

(define (compare-step-sequences actual expected)
  (cond [(and (pair? expected) (pair? actual))
         (begin (compare-steps (car actual) (car expected))
                (compare-step-sequences (cdr actual) (cdr expected)))]
        [(pair? expected)
         (fail (format "missing expected steps:\n~s" expected))]
        [(pair? actual)
         (fail (format "too many steps:\n~a"
                       (apply append
                              (for/list ([step actual])
                                (format "~s: ~s\n"
                                        (protostep-type step)
                                        (stx->datum (step-term2 step)))))))]
        [else 'ok]))

(define (compare-steps actual expected)
  (match expected
    ['error
     (check-pred misstep? actual)]
    [(list 'remark e-tag e-forms ...)
     (check-pred remarkstep? actual)
     (check-eq? (protostep-type actual) e-tag "Remark step type")
     (let ([contents (filter syntax? (remarkstep-contents actual))])
       (check-equal? (length contents) (length e-forms)
                     "Wrong number of syntaxes in remark")
       (for ([astx contents] [e-form e-forms])
         (check-equal-syntax? (syntax->datum astx) e-form "Syntax in remark")))]
    [(list e-tag e-form e-locals ...)
     (let ([lctx-terms (map bigframe-term (state-lctx (protostep-s1 actual)))])
       (check-pred step? actual)
       (check-eq? (protostep-type actual) e-tag)
       (check-equal-syntax? (syntax->datum (step-term2 actual))
                            e-form)
       (check-equal? (length lctx-terms) (length e-locals)
                     "Wrong number of context frames")
       (for ([lctx-term lctx-terms] [e-local e-locals])
         (check-equal-syntax? (syntax->datum lctx-term)
                              e-local
                              "Context frame")))]))

(define-binary-check (check-equal-syntax? a e)
  (equal-syntax? a e))

(define (equal-syntax? a e)
  (cond [(and (pair? a) (pair? e))
         (and (equal-syntax? (car a) (car e))
              (equal-syntax? (cdr a) (cdr e)))]
        [(and (symbol? a) (symbol? e))
         (equal? (symbol->string a)
                 (symbol->string e))]
        [(and (symbol? a) (regexp? e))
         (regexp-match? e (symbol->string a))]
        [else (equal? a e)]))
