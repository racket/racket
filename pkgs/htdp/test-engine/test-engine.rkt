#lang racket/base

(require racket/class
         "test-info.scm")

(define test-display-base%
  (class* object% ()
    
    (init-field (current-rep #f))
    (define test-info #f)
    
    (define/pubment (install-info t)
      (set! test-info t)
      (inner (void) install-info t))
    (define/public (get-info) test-info)
    
    (define/public (display-results)
      '...)
    
    (super-instantiate ())))

(define test-display-textual%
  (class* object% ()

    (init-field (current-rep #f))

    (define test-info #f)
    (define/pubment (install-info t)
      (set! test-info t)
      (inner (void) install-info t))

    (define/public (display-results)
      (insert-test-results test-info))

    (define/pubment (insert-test-results test-info)
      (let* ([style (send test-info test-style)]
             [total-tests (send test-info tests-run)]
             [failed-tests (send test-info tests-failed)]
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]
             [test-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond [(zero? total-tests) zero-message]
                              [(= 1 total-tests) "Ran 1 test.\n"]
                              [else (format "Ran ~a tests.\n" total-tests)]))
                (when (> total-tests 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-tests) (= 1 total-tests))
                             "Test passed!\n\n"]
                            [(zero? failed-tests) "All tests passed!\n\n"]
                            [(= failed-tests total-tests) "0 tests passed.\n"]
                            [else "~a of the ~a tests failed.\n\n"]))))]
             [check-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond
                          [(zero? total-checks) zero-message]
                          [(= 1 total-checks) "Ran 1 check.\n"]
                          [else (format "Ran ~a checks.\n" total-checks)]))
                (when (> total-checks 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-checks) (= 1 total-checks))
                             "Check passed!\n\n"]
                            [(zero? failed-checks) "All checks passed!\n\n"]
                            [(= failed-checks total-checks) "0 checks passed.\n"]
                            [else (format "~a of the ~a checks failed.\n\n"
                                          failed-checks total-checks)]))))])
        (case style
          [(test-require)
           (test-outcomes "This program must be tested!\n")
           (check-outcomes "This program is unchecked!\n")]
          [(check-require)
           (check-outcomes "This program is unchecked!\n")]
          [(test-basic)
           (test-outcomes "")
           (check-outcomes "")]
          [else (check-outcomes "")])

        (unless (and (zero? total-checks) (zero? total-tests))
          (inner (display-check-failures (send test-info failed-checks)
                                         test-info)
                 insert-test-results test-info))))

    (define/public (display-check-failures checks test-info)
      (for ([failed-check (reverse checks)])
        (printf "~a" "\t")
        (make-link (failed-check-reason failed-check)
                   (check-fail-src (failed-check-reason failed-check)))
        (printf "~a" "\n")))

    (define/public (report-success) (void))

    (define/public (display-success-summary port count)
      (unless (test-silence)
        (fprintf port "~a test~a passed!\n"
                 (case count
                   [(0) "Zero"]
                   [(1) "The only"]
                   [(2) "Both"]
                   [else (format "All ~a" count)])
                 (if (= count 1) "" "s"))))
    
    (define/public (display-untested-summary port)
      (unless (test-silence)
        (fprintf port "This program should be tested.\n")))

    (define/public (display-disabled-summary port)
      (fprintf port "Tests disabled.\n"))

    (define/public (next-line) (printf "~a" "\n\t"))

    ;; make-link: (listof (U check-fail (U string snip%))) src -> void
    (define (make-link reason dest)
      (print-reason display display reason)
      (printf "~a" (format-src dest)))

    (define (format-src src)
      (let ([src-file car]
            [src-line cadr]
            [src-col caddr])
        (string-append
         (cond [(symbol? (src-file src)) " At "]
               [(path? (src-file src))
                (string-append " In " (path->string (src-file src)) " at ")]
               [else " At "])
         "line " (cond [(src-line src) => number->string]
                       [else "(unknown)"])
         " column " (cond [(src-col src) => number->string]
                          [else "(unknown)"]))))

      (super-instantiate ())))

(define test-engine%
  (class* object% ()
    (field [test-info #f]
           [test-display #f])

    (define display-class test-display-textual%)
    (define display-rep #f)
    (define display-event-space #f)
    (define silent-mode #t)
    (define initial-report-done #f)
    (define unreported-tests #f)

    (super-instantiate ())

    (define/public (refine-display-class d) (set! display-class d))
    (define/public (info-class) test-info-base%)
    (define/public (set-silence! t) (set! silent-mode t))

    (define/public (add-analysis a) (send test-info add-analysis a))

    (define/public (setup-info style)
      (set! initial-report-done #f)
      (set! test-info (make-object (info-class) style)))
    (define/pubment (setup-display cur-rep event-space)
      (set! test-display (make-object display-class cur-rep))
      (set! display-rep cur-rep)
      (set! display-event-space event-space)
      (inner (void) setup-display cur-rep event-space))

    (define/pubment (run)
      (when (test-execute)
        (unless test-info (setup-info 'check-base))
        (inner (void) run)))

    (define/private (clear-results event-space)
      (when event-space
	(parameterize ([(dynamic-require 'mred/mred 'current-eventspace) event-space])
	  ((dynamic-require 'mred/mred 'queue-callback)
	   (lambda () (send test-display report-success))))))

    (define/public (summarize-results port)
      (cond
       ((and initial-report-done
	     (not unreported-tests)
	     (not (send test-info has-unreported-failures))))
       ((test-execute)
        (unless test-display (setup-display #f #f))
	(send test-display install-info test-info)
	(if (pair? (send test-info failed-signatures))
	    (send this display-results display-rep display-event-space)
	    (let ((result (send test-info summarize-results)))
	      (case result
		[(no-tests)
		 (clear-results display-event-space)
		 (display-untested port)]
		[(all-passed) (display-success port display-event-space
					       (+ (send test-info tests-run)
						  (send test-info checks-run)))]
		[(mixed-results)
		 (display-results display-rep display-event-space)])))
	(send test-info clear-unreported-failures)
	(set! initial-report-done #t)
	(set! unreported-tests #f))
       (else
	(display-disabled port))))

    (define/private (display-success port event-space count)
      (clear-results event-space)
      (send test-display display-success-summary port count))

    (define/public (display-results rep event-space)
      (cond
       [(and rep event-space)
	(parameterize ([(dynamic-require 'mred/mred 'current-eventspace) event-space])
	  ((dynamic-require 'mred/mred 'queue-callback)
	   (lambda () (send rep display-test-results test-display))))]
       [event-space 
	(parameterize ([(dynamic-require 'mred/mred 'current-eventspace) event-space])
	  ((dynamic-require 'mred/mred 'queue-callback) (lambda () (send test-display display-results))))]
       [else (send test-display display-results)]))

    (define/public (display-untested port)
      (when (not silent-mode)
	(send test-display display-untested-summary port)))

    (define/public (display-disabled port)
      (send test-display display-disabled-summary port))

    (define/pubment (initialize-test test)
      (inner (void) initialize-test test))

    (define/pubment (run-test test)
      (set! unreported-tests #t)
      (inner (void) run-test test))

    (define/pubment (run-testcase testcase)
      (inner (void) run-testcase testcase))))

(define test-format (make-parameter (lambda (v) (format "~a" v))))
(define test-execute (make-parameter #t))
(define error-handler (make-parameter (lambda (e) (e))))
(define test-silence (make-parameter #f))

(provide test-engine% test-display-textual% test-format error-handler test-execute test-silence)
