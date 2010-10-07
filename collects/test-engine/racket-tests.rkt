#lang mzscheme

(require lang/private/teachprims
         scheme/class
         scheme/match
	 lang/private/continuation-mark-key
         (only scheme/base for memf findf)
         "test-engine.rkt"
	 "test-info.scm"
         )

(require-for-syntax stepper/private/shared)

(provide
 check-expect ;; syntax : (check-expect <expression> <expression>)
 check-within ;; syntax : (check-within <expression> <expression> <expression>)
 check-member-of ;; syntax : (check-member-of <expression> <expression>)
 check-range ;; syntax : (check-range <expression> <expression> <expression>)
 check-error  ;; syntax : (check-error <expression> [<expression>])
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
(define LIST-FMT
  "check-member-of requires a list for the second argument, containing the possible outcomes. Given ~s")
(define CHECK-MEMBER-OF-FUNCTION-FMT
  "check-member-of cannot compare functions.")
(define RANGE-MIN-FMT
  "check-range requires a number for the minimum value. Given ~a")
(define RANGE-MAX-FMT
  "check-range requires a number for the maximum value. Given ~a")
(define CHECK-RANGE-FUNCTION-FMT
  "check-range cannot compare functions.")


(define-for-syntax CHECK-EXPECT-STR
  "check-expect requires two expressions. Try (check-expect test expected).")
(define-for-syntax CHECK-ERROR-STR
  "check-error requires at least one expression. Try (check-error test message) or (check-error test).")
(define-for-syntax CHECK-WITHIN-STR
  "check-within requires three expressions. Try (check-within test expected range).")
(define-for-syntax CHECK-MEMBER-OF-STR
  "check-member-of requires at least two expressions. Try (check-member-of test option options ...).")
(define-for-syntax CHECK-RANGE-STR
  "chech-range requires three expressions. Try (check-range test min max).")

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
             #`(let ([test-engine (namespace-variable-value
				   'test~object #f builder (current-namespace))])
                 (when test-engine
                   (insert-test test-engine
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
                                           #'test-engine))))))))
             'stepper-skipto
             (append skipto/third ;; let
                     skipto/third skipto/second ;; unless (it expands into a begin)
                     skipto/cdr skipto/third ;; application of insert-test
                     '(syntax-e cdr cdr syntax-e car) ;; lambda
                     )))
      #`(begin
          (let ([test-engine (namespace-variable-value
                            'test~object #f builder (current-namespace))])
            (when test-engine
              (begin
                (send test-engine reset-info)
                (insert-test test-engine
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
                                        #'test-engine)))))))))
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

;; check-values-expected: (-> scheme-val) scheme-val src test-engine -> void
(define (check-values-expected test actual src test-engine)
  (error-check (lambda (v) (if (number? v) (exact? v) #t))
               actual INEXACT-NUMBERS-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) actual FUNCTION-FMT #f)
  (send (send test-engine get-info) add-check)
  (run-and-check (lambda (v1 v2 _) (beginner-equal? v1 v2))
                 (lambda (src format v1 v2 _) (make-unequal src format v1 v2))
                 test actual #f src test-engine 'check-expect))

;;check-within
(define-syntax (check-within stx)
  (unless (check-context?)
    (raise-syntax-error 'check-within CHECK-WITHIN-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test actual within)
     (check-expect-maker stx #'check-values-within #`test (list #`actual #`within)
                         'comes-from-check-within)]
    [_ (raise-syntax-error 'check-within CHECK-WITHIN-STR stx)]))

;; check-values-within: (-> scheme-val) scheme-val number src test-engine -> void
(define (check-values-within test actual within src test-engine)
  (error-check number? within CHECK-WITHIN-INEXACT-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) actual CHECK-WITHIN-FUNCTION-FMT #f)
  (send (send test-engine get-info) add-check)
  (run-and-check beginner-equal~? make-outofrange test actual within src
                 test-engine
                 'check-within))

;; check-error
(define-syntax (check-error stx)
  (unless (check-context?)
    (raise-syntax-error 'check-error CHECK-ERROR-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test error)
     (check-expect-maker stx #'check-values-error #`test (list #`error)
                         'comes-from-check-error)]
    [(_ test)
     (check-expect-maker stx #'check-values-error/no-string #`test null
                         'comes-from-check-error)]
    [_ (raise-syntax-error 'check-error CHECK-ERROR-STR stx)]))

;; check-values-error: (-> scheme-val) scheme-val src test-engine -> void
(define (check-values-error test error src test-engine)
  (error-check string? error CHECK-ERROR-STR-FMT #t)
  (send (send test-engine get-info) add-check)
  (let ([result (with-handlers ([exn?
                                 (lambda (e)
                                   (or (equal? (exn-message e) error)
                                       (make-incorrect-error src (test-format) error
                                                             (exn-message e) e)))])
                  (let ([test-val (test)])
                    (make-expected-error src (test-format) error test-val)))])
    (if (check-fail? result)
        (begin
          (send (send test-engine get-info) check-failed
                result (check-fail-src result)
                (and (incorrect-error? result) (incorrect-error-exn result)))
          #f)
        #t)))

;; check-values-error/no-string: (-> scheme-val) src test-engine -> void
(define (check-values-error/no-string test src test-engine)
  (send (send test-engine get-info) add-check)
  (let ([result (with-handlers ([exn?
                                 (lambda (e) #t)])
                  (let ([test-val (test)])
                    (make-expected-an-error src (test-format) test-val)))])
    (if (check-fail? result)
        (begin
          (send (send test-engine get-info) check-failed
                result (check-fail-src result)
                #f)
          #f)
        #t)))


;;error-check: (scheme-val -> boolean) format-string boolean) -> void : raise exn:fail:contract
(define (error-check pred? actual fmt fmt-act?)
  (unless (pred? actual)
    (raise (make-exn:fail:contract (if fmt-act? (format fmt actual) fmt)
                                   (current-continuation-marks)))))

;;check-member-of
(define-syntax (check-member-of stx)
  (unless (check-context?)
    (raise-syntax-error 'check-member-of CHECK-EXPECT-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test actual actuals ...)
     (check-expect-maker stx #'check-member-of-values-expected #`test (list #`actual #`(list actuals ...))
                         'comes-from-check-member-of)]
    [_ (raise-syntax-error 'check-member-of CHECK-MEMBER-OF-STR stx)]))

;; check-member-of-values-expected: (-> scheme-val) scheme-val src test-engine -> void
(define (check-member-of-values-expected test first-actual actuals src test-engine)
  (error-check (lambda (v) (not (procedure? v))) first-actual CHECK-MEMBER-OF-FUNCTION-FMT #f)
  (send (send test-engine get-info) add-check)
  (run-and-check (lambda (v2 v1 _) (memf (lambda (i) (beginner-equal? v1 i)) v2))
                 (lambda (src format v1 v2 _) (make-not-mem src format v1 v2))
                 test (cons first-actual actuals) #f src test-engine 'check-member-of))

;;check-range
(define-syntax (check-range stx)
  (unless (check-context?)
    (raise-syntax-error 'check-member-of CHECK-EXPECT-DEFN-STR stx))
  (syntax-case stx ()
    [(_ test min max)
     (check-expect-maker stx #'check-range-values-expected #`test (list #`min #`max)
                         'comes-from-check-range)]
    [_ (raise-syntax-error 'check-range CHECK-RANGE-STR stx)]))

;; check-range-values-expected: (-> scheme-val) scheme-val src test-engine -> void
(define (check-range-values-expected test min max src test-engine)
  (error-check number? min RANGE-MIN-FMT #t)
  (error-check number? max RANGE-MAX-FMT #t) 
  (error-check (lambda (v) (not (procedure? v))) min CHECK-RANGE-FUNCTION-FMT #f)
  (error-check (lambda (v) (not (procedure? v))) max CHECK-RANGE-FUNCTION-FMT #f)
  (send (send test-engine get-info) add-check)
  (run-and-check (lambda (v2 v1 v3) (and (number? v1) (and (<= v2 v1) (<= v1 v3))))
                 (lambda (src format v1 v2 v3) (make-not-range src format v1 v2 v3))
                 test min max src test-engine 'check-range))

  
;; run-and-check: (scheme-val scheme-val scheme-val -> boolean)
;;                (src format scheme-val scheme-val scheme-val -> check-fail)
;;                ( -> scheme-val) scheme-val scheme-val test-engine symbol? -> void
(define (run-and-check check maker test expect range src test-engine kind)
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
           (send (send test-engine get-info) check-failed result (check-fail-src result) exn)
           (if exn
	       (raise exn)
	       #f)]
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
      #'(test*)
      'test-call #t)]))

(define (test*)
  (dynamic-wind
   values
   (lambda () (run-tests))
   (lambda () (display-results))))

(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(run)
      'test-call #t)]))

(define (run) 
  (let ([test-engine
         (namespace-variable-value 'test~object #f builder (current-namespace))]) 
    (and test-engine (send test-engine run))))

(define (display-results*)
  (let ([test-engine (namespace-variable-value 'test~object #f builder (current-namespace))])
    (and test-engine
         (let ([display-data (scheme-test-data)])
           (when (caddr display-data)
             (send test-engine refine-display-class (caddr display-data)))
           (send test-engine setup-display (car display-data) (cadr display-data))
           (send test-engine summarize-results (current-output-port))))))

(define-syntax (display-results stx) 
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(display-results*)
      'test-call #t)]))

(provide run-tests display-results test builder)

(define (build-test-engine)
  (let ([engine (make-object scheme-test%)])
    (send engine setup-info 'test-check)
    engine))

(define (insert-test test-engine test) (send test-engine add-test test))

(define scheme-test-data (make-parameter (list #f #f #f)))

(define signature-test-info%
  (class* test-info-base% ()
	 
    (define signature-violations '())

    (define/pubment (signature-failed obj signature message blame)

      (let* ((cms
	      (continuation-mark-set->list (current-continuation-marks)
					   teaching-languages-continuation-mark-key))
	     (srcloc
	      (cond
	       ((findf (lambda (mark)
			 (and mark
			      (or (path? (car mark))
				  (symbol? (car mark)))))
		       cms)
		=> (lambda (mark)
		     (apply (lambda (source line col pos span)
			      (make-srcloc source line col pos span))
			    mark)))
	       (else #f)))
	     (message
	      (or message
		  (make-signature-got obj (test-format)))))
		  
	(set! signature-violations
	      (cons (make-signature-violation obj signature message srcloc blame)
		    signature-violations)))
      (inner (void) signature-failed obj signature message))

    (define/public (failed-signatures) (reverse signature-violations))
    
    (inherit add-check-failure)
    (define/pubment (property-failed result src-info)
      (add-check-failure (make-property-fail src-info (test-format) result) #f))

    (define/pubment (property-error exn src-info)
      (add-check-failure (make-property-error src-info (test-format) (exn-message exn) exn) exn))

    (super-instantiate ())))

(define scheme-test%
  (class* test-engine% ()
    (super-instantiate ())
    (inherit-field test-info test-display)
    (inherit setup-info)

    (field [tests null]
           [test-objs null])

    (define/override (info-class) signature-test-info%)

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

(provide scheme-test-data test-format test-execute test-silence error-handler 
	 signature-test-info% build-test-engine)
