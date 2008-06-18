#lang mzscheme

(require lang/private/teachprims
         scheme/class
         scheme/match
         (only scheme/base for)
         "test-engine.scm"
         )

(require-for-syntax stepper/private/shared)

(provide
 check-expect ;; syntax : (check-expect <expression> <expression>)
 check-within ;; syntax : (check-within <expression> <expression> <expression>)
 check-error  ;; syntax : (check-error <expression> <expression>)
 )

(define INEXACT-NUMBERS-FMT
  "check-expect cannot compare inexact numbers. Try (check-within test ~a range).")
(define CHECK-ERROR-STR-FMT
  "check-error requires a string for the second argument, representing the expected error message. Given ~s")
(define CHECK-WITHIN-INEXACT-FMT
  "check-within requires an inexact number for the range. ~a is not inexact.")

(define-for-syntax CHECK-EXPECT-STR
  "check-expect requires two expressions. Try (check-expect test expected).")
(define-for-syntax CHECK-ERROR-STR
  "check-error requires two expressions. Try (check-error test message).")
(define-for-syntax CHECK-WITHIN-STR
  "check-within requires three expressions. Try (check-within test expected range).")

(define-for-syntax CHECK-EXPECT-DEFN-STR
  "found a test that is not at the top level")
(define-for-syntax CHECK-WITHIN-DEFN-STR
  CHECK-EXPECT-DEFN-STR)
(define-for-syntax CHECK-ERROR-DEFN-STR
  CHECK-EXPECT-DEFN-STR)

(define-struct check-fail (src))

;; (make-unexpected-error src string)
(define-struct (unexpected-error check-fail) (expected message))
;; (make-unequal src scheme-val scheme-val)
(define-struct (unequal check-fail) (test actual))
;; (make-outofrange src scheme-val scheme-val inexact)
(define-struct (outofrange check-fail) (test actual range))
;; (make-incorrect-error src string)
(define-struct (incorrect-error check-fail) (expected message))
;; (make-expected-error src string scheme-val)
(define-struct (expected-error check-fail) (message value))

;; check-expect-maker : syntax? syntax? (listof syntax?) -> syntax?
;; the common part of all three test forms.
(define-for-syntax (check-expect-maker stx checker-proc-stx embedded-stxes hint-tag)
  (with-syntax ([bogus-name (stepper-syntax-property #`#,(gensym 'test) 'stepper-hide-completed #t)]
                [src-info (with-stepper-syntax-properties (['stepper-skip-completely #t])
                            #`(list #,@(list #`(quote #,(syntax-source stx))
                                             (syntax-line stx)
                                             (syntax-column stx)
                                             (syntax-position stx)
                                             (syntax-span stx))))])
    (quasisyntax/loc stx
      (define bogus-name
        #,(stepper-syntax-property
           #`(let ([test-info (namespace-variable-value
                               'test~object #f builder (current-namespace))])
               (when test-info
                 (insert-test test-info
                              (lambda ()
                                #,(with-stepper-syntax-properties (['stepper-hint hint-tag]
                                                                   ['stepper-hide-reduction #t]
                                                                   ['stepper-use-val-as-final #t])
                                    (quasisyntax/loc stx
                                      (#,checker-proc-stx
                                       #,@embedded-stxes 
                                       src-info
                                       #,(with-stepper-syntax-properties (['stepper-no-lifting-info #t]
                                                                          ['stepper-hide-reduction #t])
                                           #'test-info))))))))
           'stepper-skipto
           (append skipto/third ;; let 
                   skipto/third skipto/second ;; unless (it expands into a begin)
                   skipto/cdr skipto/third ;; application of insert-test
                   '(syntax-e cdr cdr syntax-e car) ;; lambda
                   ))))))

(define-for-syntax (check-context?)
  (let ([c (syntax-local-context)])
    (memq c '(module top-level))))

;; check-expect
(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ test actual)
     (check-context?)
     (check-expect-maker stx #'check-values-expected (list #`(lambda () test) #`actual) 'comes-from-check-expect)]
    [(_ test)
     (check-context?)
     (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx)]
    [(_ test actual extra ...)
     (check-context?)
     (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx)]
    [(_ test ...)
     (not (check-context?))
     (raise-syntax-error 'check-expect CHECK-EXPECT-DEFN-STR stx)]))

;; check-values-expected: (-> scheme-val) scheme-val src -> void
(define (check-values-expected test actual src test-info)
  (error-check (lambda (v) (if (number? v) (exact? v) #t))
               actual INEXACT-NUMBERS-FMT)
  (send (send test-info get-info) add-check)
  (run-and-check (lambda (v1 v2 _) (beginner-equal? v1 v2))
                 (lambda (src v1 v2 _) (make-unequal src v1 v2))
                 test actual #f src test-info 'check-expect))

(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ test actual within)
     (check-context?)
     (check-expect-maker stx #'check-values-within (list #`(lambda () test) #`actual #`within) 'comes-from-check-within)]
    [(_ test actual)
     (check-context?)
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test)
     (check-context?)
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test actual within extra ...)
     (check-context?)
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test ...)
     (not (check-context?))
     (raise-syntax-error 'check-within CHECK-WITHIN-DEFN-STR stx)]))

(define (check-values-within test actual within src test-info)
  (error-check number? within CHECK-WITHIN-INEXACT-FMT)
  (send (send test-info get-info) add-check)
  (run-and-check beginner-equal~? make-outofrange test actual within src
                 test-info
                 'check-within))


(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ test error)
     (check-context?)
     (check-expect-maker stx #'check-values-error (list #'(lambda () test) #`error) 'comes-from-check-error)]
    [(_ test)
     (check-context?)
     (raise-syntax-error 'check-error CHECK-ERROR-STR stx)]
    [(_ test ...)
     (not (check-context?))
     (raise-syntax-error 'check-error CHECK-ERROR-DEFN-STR stx)]))

(define (check-values-error test error src test-info)
  (error-check string? error CHECK-ERROR-STR-FMT)
  (send (send test-info get-info) add-check)
  (let ([result (with-handlers ([exn?
                                 (lambda (e)
                                   (or (equal? (exn-message e) error)
                                       (make-incorrect-error src error
                                                             (exn-message e))))])
                  (let ([test-val (test)])
                    (make-expected-error src error test-val)))])
    (if (check-fail? result)
        (begin
          (send (send test-info get-info) check-failed
                (check->message result) (check-fail-src result))
          (list 'check-error-failed 
                (if (expected-error? result)
                    (expected-error-message result)
                    (incorrect-error-message result))
                error))
        (list 'check-error-succeeded error error))))


(define (error-check pred? actual fmt)
  (unless (pred? actual)
    (raise (make-exn:fail:contract (format fmt actual)
                                   (current-continuation-marks)))))




;; run-and-check: (scheme-val scheme-val scheme-val -> boolean)
;;                (scheme-val scheme-val scheme-val -> check-fail)
;;                ( -> scheme-val) scheme-val scheme-val object symbol? -> void
(define (run-and-check check maker test expect range src test-info kind)
  (match-let ([(list result result-val)
               (with-handlers ([exn? (lambda (e)
                                       (list (make-unexpected-error src expect
                                                                    (exn-message e)) 'error))])
                 (let ([test-val (test)])
                   (cond [(check expect test-val range) (list #t test-val)]
                         [else 
                          (list (maker src test-val expect range) test-val)])))])
    (cond [(check-fail? result)
           (send (send test-info get-info) check-failed (check->message result) (check-fail-src result))
           (render-for-stepper/fail result expect range kind)]
          [else 
           ;; I'd like to pass the actual, but I don't have it.
           (render-for-stepper/pass result-val expect range kind)])))

;; render-for-stepper/fail : test-fail? any/c any/c symbol? -> any/c
;; arrange the fail-result as a value that will look tolerable
;;  in the stepper's list of completed expressions
(define (render-for-stepper/fail check-fail expected range kind)
  (let ([displayed-result (cond [(unexpected-error? check-fail) 'error]
                                ;; I really want actual here, but I'm using test because the thing is 
                                ;; created backward, afaict.
                                [(unequal? check-fail) (unequal-test check-fail)]
                                [(outofrange? check-fail) (outofrange-test check-fail)])])
    (case kind
      [(check-expect) (list 'check-expect-failed displayed-result expected)]
      [(check-within) (list 'check-within-failed displayed-result expected range)]
      [else (error 'render-for-stepper/fail "internal error 2008052801")])))

;; render-for-stepper/pass : any/c any/c any/c symbol? -> any/c
(define (render-for-stepper/pass actual expected range kind)
  (case kind
    [(check-expect) (list 'check-expect-passed actual expected)]
    [(check-within) (list 'check-within-passed actual expected range)]
    [else (error 'render-for-stepper/pass "internal error 2008052802")]))

(define (check->message fail)
  (cond
    [(unexpected-error? fail)
     (list "check encountered the following error instead of the expected value, "
           ((test-format) (unexpected-error-expected fail))
           (format ". ~n   :: ~a~n" (unexpected-error-message fail)))]
    [(unequal? fail)
     (list "Actual value "
           ((test-format) (unequal-test fail))
           " differs from "
           ((test-format) (unequal-actual fail))
           ", the expected value.\n")]
    [(outofrange? fail)
     (list "Actual value "
           ((test-format) (outofrange-test fail))
           (format " is not within ~v of expected value " (outofrange-range fail))
           ((test-format) (outofrange-actual fail))
           ".\n")]
    [(incorrect-error? fail)
     (list (format "check-error encountered the following error instead of the expected ~a~n   :: ~a ~n"
                   (incorrect-error-expected fail)
                   (incorrect-error-message fail)))]
    [(expected-error? fail)
     (list "check-error expected the following error, but instead received the value "
           ((test-format) (expected-error-value fail))
           (format ".~n ~a~n" (expected-error-message fail)))]))


(define (builder)
  (let ([te (build-test-engine)])
    (namespace-set-variable-value! 'test~object te (current-namespace))
    te))

(define (test) (run-tests) (display-results))

(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(run (namespace-variable-value 'test~object #f builder (current-namespace)))
      'test-call #t)]))

(define (run test-info) (and test-info (send test-info run)))

(define-syntax (display-results stx) 
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(let ([test-info (namespace-variable-value 'test~object #f builder (current-namespace))])
          (and test-info
               (let ([display-data (scheme-test-data)])
                 (when (caddr display-data)
                   (send test-info refine-display-class (caddr display-data)))
                 (send test-info setup-display (car display-data) (cadr display-data))
                 (send test-info summarize-results (current-output-port)))))
      'test-call #t)]))

(provide run-tests display-results test builder)

(define (build-test-engine)
  (let ([engine (make-object scheme-test%)])
    (send engine setup-info 'check-require)
    engine))

(define (insert-test test-info test) (send test-info add-test test))

(define scheme-test-data (make-parameter (list #f #f #f)))

(define scheme-test%
  (class* test-engine% ()
    (super-instantiate ())
    (inherit-field test-info test-display)
    (inherit setup-info)

    (field [tests null]
           [test-objs null])

    (define/public (add-test tst)
      (set! tests (cons tst tests)))
    (define/public (get-info)
      (unless test-info (send this setup-info 'check-require))
      test-info)

    (define/augment (run)
      (inner (void) run)
      (for ([t (reverse tests)]) (run-test t)))

    (define/augment (run-test test)
      (test)
      (inner (void) run-test test))))

(provide scheme-test-data test-format test-execute test-silence)