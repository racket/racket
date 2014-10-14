#lang scheme/base

(require scheme/class
         deinprogramm/quickcheck/quickcheck
         "print.ss")

(provide (all-defined-out))

;; (make-failed-check check-fail (U #f exn)
(define-struct failed-check (reason exn?))

(define-struct check-fail (src format))

;; (make-unexpected-error src format string exn)
(define-struct (unexpected-error check-fail) (expected message exn))
(define-struct (unsatisfied-error check-fail) (expected message exn))
;; (make-unequal src format scheme-val scheme-val)
(define-struct (unequal check-fail) (test actual))
;; (make-outofrange src format scheme-val scheme-val inexact)
(define-struct (outofrange check-fail) (test actual range))
;; (make-incorrect-error src format string exn)
(define-struct (incorrect-error check-fail) (expected message exn))
;; (make-expected-error src format string scheme-val)
(define-struct (expected-error check-fail) (message value))
;; (make-expected-an-error src format scheme-val)
(define-struct (expected-an-error check-fail) (value))
;; (make-not-mem src format scheme-val scheme-val)
(define-struct (not-mem check-fail) (test set))
;; (make-not-range src format scheme-val scheme-val scheme-val)
(define-struct (not-range check-fail) (test min max))
;; (make-satisfied-failed src format scheme-val symbol)
(define-struct (satisfied-failed check-fail) (actual name))

;;Wishes
(define-struct (unimplemented-wish check-fail) (name args))


(define-struct signature-got (value format))

(define-struct signature-violation (obj signature message srcloc blame))

(define-struct (property-fail check-fail) (result))
(define-struct (property-error check-fail) (message exn))

;; (make-message-error src format (listof string))
(define-struct (message-error check-fail) (strings))

(define test-info-base%
  (class* object% ()
    (super-instantiate ())
    
    (init-field (style 'check-base))
    (field [analyses null])
    
    (define total-tsts 0)
    (define failed-tsts 0)
    (define total-cks 0)
    (define failed-cks 0)
    (define total-called-wishes 0)
    
    (define failures null)
    (define wishes null)
    
    (define unreported-failures #f)
    
    (define/public (clear-unreported-failures)
      (set! unreported-failures #f))
    
    (define/public (report-failure)
      (set! unreported-failures #t))
    
    (define/public (has-unreported-failures)
      unreported-failures)
    
    (define/public (test-style) style)
    (define/public (tests-run) total-tsts)
    (define/public (tests-failed) failed-tsts)
    (define/public (checks-run) total-cks)
    (define/public (checks-failed) failed-cks)
    (define/public (summarize-results)
      (cond [(and (zero? total-tsts) (zero? total-cks)) 'no-tests]
            [(and (zero? failed-cks) (zero? failed-tsts)) 'all-passed]
            [else 'mixed-results]))
    (define/public (called-wishes) total-called-wishes)
    
    (define/public (failed-checks) failures)
    (define/public (unimplemented-wishes) wishes)
    
    (define/pubment (add-wish-call name)
      (set! total-called-wishes (add1 total-called-wishes))
      (unless (memq name wishes) (set! wishes (cons name wishes)))
      (inner (void) add-wish-call name))
    
    (define/pubment (add-check)
      (set! total-cks (add1 total-cks))
      (inner (void) add-check))
    
    (define/pubment (add-test)
      (set! total-tsts (add1 total-tsts))
      (inner (void) add-test))
    
    (define/pubment (add-check-failure fail exn?)
      (set! failed-cks (add1 failed-cks))
      (set! failures (cons (make-failed-check fail exn?) failures))
      (inner (void) add-check-failure fail exn?))
    
    (define/pubment (add-wish name)
      (unless (memq name wishes)
        (set! wishes (cons name wishes)))
      (inner (void) add-wish name))
    
    ;; check-failed: (U check-fail (list (U string snip%))) src (U exn false) -> void
    (define/pubment (check-failed msg src exn?)
      (let ((fail
             ;; We'd like every caller to make a check-fail object,
             ;; but some (such as ProfessorJ's run time) cannot because
             ;; of phase problems.  Therefore, do the coercion here.
             (if (check-fail? msg)
                 msg
                 (make-message-error src #f msg))))
        (add-check-failure fail exn?)
        (report-failure)
        (inner (void) check-failed fail src exn?)))
    
    (define/pubment (test-failed failed-info)
      (set! failed-tsts (add1 failed-tsts))
      (report-failure)
      (inner (void) test-failed failed-info))
    
    (define/public (add-analysis a) (set! analyses (cons a analyses)))
    
    (define/public (analyze-position src . vals)
      (for ([a analyses]) (send a analyze src vals)))
    (define/public (extract-info pred?)
      (filter pred? (map (lambda (a) (send a provide-info)) analyses)))))

; helper for printing error messages
(define (print-reason print-string print-formatted fail)
  (let ((print
         (lambda (fstring . vals)
           (apply print-with-values fstring print-string print-formatted vals)))
        (formatter (check-fail-format fail)))
    (cond
      [(unsatisfied-error? fail)
       (print 
        "check-satisfied encountered an error instead of the expected kind of value, ~F. \n   :: ~a"
        (formatter (unsatisfied-error-expected fail))
        (unsatisfied-error-message fail))]
      [(unexpected-error? fail)
       (print
        "check-expect encountered the following error instead of the expected value, ~F. \n   :: ~a"
        (formatter (unexpected-error-expected fail))
        (unexpected-error-message fail))]
      [(satisfied-failed? fail)
       (print "Actual value ~F does not satisfy ~F.\n"
              (formatter (satisfied-failed-actual fail))
              (formatter (satisfied-failed-name fail)))]
      [(unequal? fail)
       (print "Actual value ~F differs from ~F, the expected value."
              (formatter (unequal-test fail))
              (formatter (unequal-actual fail)))]
      [(outofrange? fail)
       (print "Actual value ~F is not within ~a of expected value ~F."
              (formatter (outofrange-test fail))
              (formatter  (outofrange-range fail))
              (formatter (outofrange-actual fail)))]
      [(incorrect-error? fail)
       (print "check-error encountered the following error instead of the expected ~a\n   :: ~a"
              (incorrect-error-expected fail)
              (incorrect-error-message fail))]
      [(expected-error? fail)
       (print "check-error expected the following error, but instead received the value ~F.\n ~a"
              (formatter (expected-error-value fail))
              (expected-error-message fail))]
      [(message-error? fail)
       (for-each print-formatted (message-error-strings fail))]
      [(not-mem? fail)
       (print "Actual value ~F differs from all given members in ~F."
              (formatter (not-mem-test fail))
              (formatter (not-mem-set fail)))]
      [(not-range? fail)
       (print "Actual value ~F is not between ~F and ~F, inclusive."
              (formatter (not-range-test fail))
              (formatter (not-range-min fail))
              (formatter (not-range-max fail)))]
      [(unimplemented-wish? fail)
       (print (string-append "Test relies on a call to wished-for function ~F "
                             " that has not been implemented, with arguments ~F.")
              (unimplemented-wish-name fail)
              (formatter (unimplemented-wish-args fail)))]
      [(property-fail? fail)
       (print-string "Property falsifiable with")
       (for-each (lambda (arguments)
                   (for-each (lambda (p)
                               (if (car p)
                                   (print " ~a = ~F" (car p) (formatter (cdr p)))
                                   (print "~F" (formatter (cdr p)))))
                             arguments))
                 (result-arguments-list (property-fail-result fail)))]
      [(property-error? fail)
       (print "check-property encountered the following error\n:: ~a"
              (property-error-message fail))])
    (print-string "\n")))
