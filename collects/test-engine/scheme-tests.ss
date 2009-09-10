#lang mzscheme

(require lang/private/teachprims
         scheme/class
         scheme/match
         (only scheme/base for)
         "test-engine.scm"
	 "test-info.scm"
         )

(require-for-syntax stepper/private/shared)

(provide
 check-expect ;; syntax : (check-expect <expression> <expression>)
 check-within ;; syntax : (check-within <expression> <expression> <expression>)
 check-error  ;; syntax : (check-error <expression> <expression>)
 )

; for other modules implementing check-expect-like forms
(provide
 (for-syntax check-expect-maker))

(define INEXACT-NUMBERS-FMT
  "check-expect cannot compare inexact numbers. Try (check-within test ~a range).")
(define FUNCTION-FMT
  "check-expect cannot compare functions.")
(define CHECK-ERROR-STR-FMT
  "check-error requires a string for the second argument, representing the expected error message. Given ~s")
(define CHECK-WITHIN-INEXACT-FMT
  "check-within requires an inexact number for the range. ~a is not inexact.")
(define CHECK-WITHIN-FUNCTION-FMT
  "check-within cannot compare functions.")

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

;; check-expect-maker : syntax? syntax? (listof syntax?) symbol? -> syntax?
;; the common part of all three test forms.
(define-for-syntax (check-expect-maker
                    stx checker-proc-stx test-expr embedded-stxes hint-tag)
  (define bogus-name
    (stepper-syntax-property #`#,(gensym 'test) 'stepper-hide-completed #t))
  (define src-info
    (with-stepper-syntax-properties (['stepper-skip-completely #t])
      #`(list #,@(list #`(quote #,(syntax-source stx))
                       (syntax-line stx)
                       (syntax-column stx)
                       (syntax-position stx)
                       (syntax-span stx)))))
  (if (eq? 'module (syntax-local-context))
      #`(define #,bogus-name
          #,(stepper-syntax-property
             #`(let ([test-info (namespace-variable-value
                                 'test~object #f builder (current-namespace))])
                 (when test-info
                   (insert-test test-info
                                (lambda ()
                                  #,(with-stepper-syntax-properties
                                     (['stepper-hint hint-tag]
                                      ['stepper-hide-reduction #t]
                                      ['stepper-use-val-as-final #t])
                                     (quasisyntax/loc stx
                                       (#,checker-proc-stx
                                        #,(with-stepper-syntax-properties
                                           (['stepper-hide-reduction #t])
                                           #`(car 
                                              #,(with-stepper-syntax-properties
                                                 (['stepper-hide-reduction #t])
                                                 #`(list
                                                    (lambda () #,test-expr)
                                                    #,(syntax/loc stx (void))))))
                                        #,@embedded-stxes
                                        #,src-info
                                        #,(with-stepper-syntax-properties
                                           (['stepper-no-lifting-info #t]
                                            ['stepper-hide-reduction #t])
                                           #'test-info))))))))
             'stepper-skipto
             (append skipto/third ;; let
                     skipto/third skipto/second ;; unless (it expands into a begin)
                     skipto/cdr skipto/third ;; application of insert-test
                     '(syntax-e cdr cdr syntax-e car) ;; lambda
                     )))
      #`(begin
          (let ([test-info (namespace-variable-value
                            'test~object #f builder (current-namespace))])
            (when test-info
              (begin
                (send test-info reset-info)
                (insert-test test-info
                             (lambda ()
                               #,(with-stepper-syntax-properties
                                  (['stepper-hint hint-tag]
                                   ['stepper-hide-reduction #t]
                                   ['stepper-use-val-as-final #t])
                                  (quasisyntax/loc stx
                                    (#,checker-proc-stx
                                     #,(with-stepper-syntax-properties
                                        (['stepper-hide-reduction #t])
                                        #`(car 
                                           #,(with-stepper-syntax-properties
                                              (['stepper-hide-reduction #t])
                                              #`(list
                                                 (lambda () #,test-expr)
                                                 #,(syntax/loc stx (void))))))
                                     #,@embedded-stxes
                                     #,src-info
                                     #,(with-stepper-syntax-properties
                                        (['stepper-no-lifting-info #t]
                                         ['stepper-hide-reduction #t])
                                        #'test-info)))))))))
          (test))))

(define-for-syntax (check-context?)
  (let ([c (syntax-local-context)])
    (memq c '(module top-level))))

;; check-expect
(define-syntax (check-expect stx)
  (unless (check-context?)
    (raise-syntax-error 'check-expect CHECK-EXPECT-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test actual)
     (check-expect-maker stx #'check-values-expected #`test (list #`actual)
                         'comes-from-check-expect)]
    [_ (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx)]))

;; check-values-expected: (-> scheme-val) scheme-val src -> void
(define (check-values-expected test actual src test-info)
  (error-check (lambda (v) (if (number? v) (exact? v) #t))
               actual INEXACT-NUMBERS-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) actual FUNCTION-FMT #f)
  (send (send test-info get-info) add-check)
  (run-and-check (lambda (v1 v2 _) (beginner-equal? v1 v2))
                 (lambda (src format v1 v2 _) (make-unequal src format v1 v2))
                 test actual #f src test-info 'check-expect))


(define-syntax (check-within stx)
  (unless (check-context?)
    (raise-syntax-error 'check-within CHECK-WITHIN-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test actual within)
     (check-expect-maker stx #'check-values-within #`test (list #`actual #`within)
                         'comes-from-check-within)]
    [_ (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]))

(define (check-values-within test actual within src test-info)
  (error-check number? within CHECK-WITHIN-INEXACT-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) actual CHECK-WITHIN-FUNCTION-FMT #f)
  (send (send test-info get-info) add-check)
  (run-and-check beginner-equal~? make-outofrange test actual within src
                 test-info
                 'check-within))


(define-syntax (check-error stx)
  (unless (check-context?)
    (raise-syntax-error 'check-error CHECK-ERROR-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test error)
     (check-expect-maker stx #'check-values-error #`test (list #`error)
                         'comes-from-check-error)]
    [_ (raise-syntax-error 'check-error CHECK-ERROR-STR stx)]))

(define (check-values-error test error src test-info)
  (error-check string? error CHECK-ERROR-STR-FMT #t)
  (send (send test-info get-info) add-check)
  (let ([result (with-handlers ([exn?
                                 (lambda (e)
                                   (or (equal? (exn-message e) error)
                                       (make-incorrect-error src (test-format) error
                                                             (exn-message e) e)))])
                  (let ([test-val (test)])
                    (make-expected-error src (test-format) error test-val)))])
    (if (check-fail? result)
        (begin
          (send (send test-info get-info) check-failed
                result (check-fail-src result)
                (and (incorrect-error? result) (incorrect-error-exn result)))
          #f)
        #t)))


(define (error-check pred? actual fmt fmt-act?)
  (unless (pred? actual)
    (raise (make-exn:fail:contract (if fmt-act? (format fmt actual) fmt)
                                   (current-continuation-marks)))))




;; run-and-check: (scheme-val scheme-val scheme-val -> boolean)
;;                (src format scheme-val scheme-val scheme-val -> check-fail)
;;                ( -> scheme-val) scheme-val scheme-val object symbol? -> void
(define (run-and-check check maker test expect range src test-info kind)
  (match-let ([(list result result-val exn)
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  (let ([display (error-display-handler)])
                                    (list (make-unexpected-error src (test-format) expect
                                                                 (exn-message e) 
                                                                 e)
                                          'error
                                          e)))])
                 (let ([test-val (test)])
                   (cond [(check expect test-val range) (list #t test-val #f)]
                         [else 
                          (list (maker src (test-format) test-val expect range) test-val #f)])))])
    (cond [(check-fail? result)
           (send (send test-info get-info) check-failed result (check-fail-src result) exn)
           (raise exn)]
          [else
           #t])))

(define (builder)
  (let ([te (build-test-engine)])
    (namespace-set-variable-value! 'test~object te (current-namespace))
    te))

(define-syntax (test stx) 
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(dynamic-wind
	    values
	    (lambda () (run-tests))
	    (lambda () (display-results)))
      'test-call #t)]))

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
    (send engine setup-info 'test-check)
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
    (define/public (reset-info)
      (set! tests null)
      #;(send this setup-info 'check-require))

    (define/augment (run)
      (inner (void) run)
      (for ([t (reverse tests)]) (run-test t)))

    (define/augment (run-test test)
      (test)
      (inner (void) run-test test))))

(provide scheme-test-data test-format test-execute test-silence error-handler)
