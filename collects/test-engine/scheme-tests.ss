#lang mzscheme

(require lang/private/teachprims
         mred
         framework
         mzlib/pretty
         mzlib/pconvert
         mzlib/class
         "scheme-gui.scm"
         "test-display.scm")

(require-for-syntax stepper/private/shared)

(provide
 check-expect ;; syntax : (check-expect <expression> <expression>)
 check-within ;; syntax : (check-within <expression> <expression> <expression>)
 check-error  ;; syntax : (check-error <expression> <expression>)
 )

(define (builder)
  (let ([te (build-test-engine)])
    (namespace-set-variable-value! 'test~object te (current-namespace))
    te))

(define (test)
  (run-tests)
  (display-results))

(define (test-text)
  (run-tests)
  (print-results))

(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(run (namespace-variable-value 'test~object #f builder
                                       (current-namespace)))
      'test-call #t)]))

(define (run test-info) (and test-info (send test-info run)))

(define-syntax (display-results stx) 
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(let ([test-info (namespace-variable-value 'test~object #f builder
                                                   (current-namespace))])
          (and test-info
               (let ([display-data (scheme-test-data)])
                 (send test-info setup-display
                       (car display-data) (cadr display-data))
                 (send test-info summarize-results (current-output-port)))))
      'test-call #t)]))

(define-syntax (print-results stx)
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(let ([test-info (namespace-variable-value 'test~object #f builder
                                                   (current-namespace))])
          (and test-info
               (send test-info refine-display-class test-display-textual%)
               (send test-info summarize-results (current-output-port))))
      'test-call #t)]))

(provide run-tests display-results test test-text)

(define (build-test-engine)
  (let ([engine (make-object scheme-test%)])
    (send engine setup-info 'check-require)
    engine))
(define (insert-test test-info test) (send test-info add-test test))

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
  "check-expect cannot be used as an expression")
(define-for-syntax CHECK-WITHIN-DEFN-STR
  "check-within cannot be used as an expression")
(define-for-syntax CHECK-ERROR-DEFN-STR
  "check-error cannot be used as an expression")

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

(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ test actual)
     (not (eq? (syntax-local-context) 'expression))
     (quasisyntax/loc stx
       (define #,(gensym 'test)
         #,(stepper-syntax-property
            #`(let ([test-info (namespace-variable-value
                                'test~object #f builder (current-namespace))])
                (when test-info
                  (insert-test test-info
                               (lambda ()
                                 (check-values-expected
                                  (lambda () test)
                                  actual
                                  (list #,@(list #`(quote #,(syntax-source stx))
                                                 (syntax-line stx)
                                                 (syntax-column stx)
                                                 (syntax-position stx)
                                                 (syntax-span stx)))
                                  test-info)))))
            `stepper-hint
            `comes-from-check-expect)))]
    [(_ test)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx)]
    [(_ test actual extra ...)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-expect CHECK-EXPECT-STR stx)]
    [(_ test ...)
     (eq? (syntax-local-context) 'expression)
     (raise-syntax-error 'check-expect CHECK-EXPECT-DEFN-STR stx)]))

;; check-values-expected: (-> scheme-val) scheme-val src -> void
(define (check-values-expected test actual src test-info)
  (error-check (lambda (v) (if (number? v) (exact? v) #t))
               actual INEXACT-NUMBERS-FMT)
  (send (send test-info get-info) add-check)
  (run-and-check (lambda (v1 v2 _) (beginner-equal? v1 v2))
                 (lambda (src v1 v2 _) (make-unequal src v1 v2))
                 test actual #f src test-info))

(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ test actual within)
     (not (eq? (syntax-local-context) 'expression))
     (quasisyntax/loc stx
       (define #,(gensym 'test-within)
         (let ([test-info (namespace-variable-value
                           'test~object #f builder (current-namespace))])
           (when test-info
             (insert test-info
                     (lambda ()
                       (check-values-within
                        (lambda () test) actual within
                        (list #,@(list (syntax-source stx)
                                       (syntax-line stx)
                                       (syntax-column stx)
                                       (syntax-position stx)
                                       (syntax-span stx)))
                        test-info)))))))]
    [(_ test actual)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test actual within extra ...)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]
    [(_ test ...)
     (eq? (syntax-local-context) 'expression)
     (raise-syntax-error 'check-within CHECK-WITHIN-DEFN-STR stx)]))

(define (check-values-within test actual within src test-info)
  (error-check number? within CHECK-WITHIN-INEXACT-FMT)
  (send (send test-info get-info) add-check)
  (run-and-check beginner-equal~? make-outofrange test actual within src
                 test-info))

(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ test error)
     (not (eq? (syntax-local-context) 'expression))
     (quasisyntax/loc stx
       (define #,(gensym 'test-error)
         (let ([test-info (namespace-variable-value
                           'test~object #f builder (current-namespace))])
           (when test-info
             (insert-test test-info
                          (lambda ()
                            (check-values-error
                             (lambda () test) error
                             (list #,@(list (syntax-source stx)
                                            (syntax-line stx)
                                            (syntax-column stx)
                                            (syntax-position stx)
                                            (syntax-span stx)))
                             test-info)))))))]
    [(_ test)
     (not (eq? (syntax-local-context) 'expression))
     (raise-syntax-error 'check-error CHECK-ERROR-STR stx)]
    [(_ test ...)
     (eq? (syntax-local-context) 'expression)
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
    (when (check-fail? result)
      (send (send test-info get-info) check-failed
            (check->message result) (check-fail-src result)))))

(define (error-check pred? actual fmt)
  (unless (pred? actual)
    (raise (make-exn:fail:contract (format fmt actual)
                                   (current-continuation-marks)))))

;; run-and-check: (scheme-val scheme-val scheme-val -> boolean)
;;                (scheme-val scheme-val scheme-val -> check-fail)
;;                ( -> scheme-val) scheme-val scheme-val object -> void
(define (run-and-check check maker test expect range src test-info)
  (let ([result
         (with-handlers ([exn? (lambda (e)
                                 (make-unexpected-error src expect
                                                        (exn-message e)))])
           (let ([test-val (test)])
             (or (check test-val expect range)
                 (maker src test-val expect range))))])
    (when (check-fail? result)
      (send (send test-info get-info) check-failed (check->message result)
            (check-fail-src result)))))

(define (check->message fail)
  (cond
    [(unexpected-error? fail)
     (list "check encountered the following error instead of the expected value, "
           (format-value (unexpected-error-expected fail))
           (format ". ~n   :: ~a~n" (unexpected-error-message fail)))]
    [(unequal? fail)
     (list "Actual value "
           (format-value (unequal-test fail))
           " differs from "
           (format-value (unequal-actual fail))
           ", the expected value.\n")]
    [(outofrange? fail)
     (list "Actual value "
           (format-value (outofrange-test fail))
           (format " is not within ~v of expected value " (outofrange-range fail))
           (format-value (outofrange-actual fail))
           ".\n")]
    [(incorrect-error? fail)
     (list (format "check-error encountered the following error instead of the expected ~a~n   :: ~a ~n"
                   (incorrect-error-expected fail)
                   (incorrect-error-message fail)))]
    [(expected-error? fail)
     (list "check-error expected the following error, but instead received the value "
           (format-value (expected-error-value fail))
           (format ".~n ~a~n" (expected-error-message fail)))]))

(define (format-value value)
  (cond
    [(is-a? value snip%) value]
    [(or (pair? value) (struct? value))
     (parameterize ([constructor-style-printing #t]
                    [pretty-print-columns 40])
       (let* ([text* (new (editor:standard-style-list-mixin text%))]
              [text-snip (new editor-snip% [editor text*])])
         (pretty-print (print-convert value) (open-output-text-editor text*))
         (send text* lock #t)
         text-snip))]
    [else (format "~v" value)]))
