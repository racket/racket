#lang scheme/base

(provide build-contract-test-engine
	 contract-violation?
	 contract-violation-obj contract-violation-contract contract-violation-message
	 contract-violation-blame contract-violation-srcloc
	 contract-got? contract-got-value contract-got-format
	 property-fail? property-fail-result
	 property-error? make-property-error property-error-message property-error-exn)

(require scheme/class
	 (lib "test-engine/test-engine.scm")
	 (lib "test-engine/test-info.scm"))

(define (build-contract-test-engine)
  (let ((engine (make-object contract-test-engine%)))
    (send engine setup-info 'check-require)
    engine))

(define contract-test-engine%
  (class* test-engine% ()
    (super-instantiate ())
    (inherit-field test-info test-display)
    (inherit setup-info display-untested display-disabled)

    (define display-rep #f)
    (define display-event-space #f)

    (field (tests null)
           (test-objs null))

    (define/override (info-class) contract-test-info%)

    ;; need display-rep & display-event-space
    (define/augment (setup-display cur-rep event-space)
      (set! display-rep cur-rep)
      (set! display-event-space event-space)
      (inner (void) setup-display cur-rep event-space))

    (define/public (add-test tst)
      (set! tests (cons tst tests)))
    (define/public (get-info)
      (unless test-info (setup-info 'check-require))
      test-info)

    (define/augment (run)
      (inner (void) run)
      (for ((t (reverse tests))) (run-test t)))

    (define/augment (run-test test)
      (test)
      (inner (void) run-test test))

    (define/private (clear-results event-space)
      (when event-space
	(parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event-space])
	  ((dynamic-require 'scheme/gui 'queue-callback)
	   (lambda () (send test-display report-success))))))

    (define/override (summarize-results port)
      (cond
       ((test-execute)
        (unless test-display (setup-display #f #f))
	(send test-display install-info test-info)
	(if (pair? (send test-info failed-contracts))
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
		 (display-results display-rep display-event-space)]))))
       (else
	(display-disabled port))))

    (define/private (display-success port event-space count)
      (clear-results event-space)
      (send test-display display-success-summary port count))

    (define/override (display-results rep event-space)
      (cond
       [(and rep event-space)
	(parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event-space])
	  ((dynamic-require 'scheme/gui 'queue-callback)
	   (lambda () (send rep display-test-results test-display))))]
       [event-space 
	(parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event-space])
	  ((dynamic-require 'scheme/gui 'queue-callback) (lambda () (send test-display display-results))))]
       [else (send test-display display-results)]))

))

(define-struct contract-got (value format))

(define-struct contract-violation (obj contract message srcloc blame))

(define-struct (property-fail check-fail) (result))
(define-struct (property-error check-fail) (message exn))

(define contract-test-info%
  (class* test-info-base% ()
	 
    (define contract-violations '())

    (define/pubment (contract-failed obj contract message blame)

      (let* ((cms
	      (continuation-mark-set->list (current-continuation-marks)
					   ;; set from deinprogramm-langs.ss
					   'deinprogramm-teaching-languages-continuation-mark-key))
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
		  (make-contract-got obj (test-format)))))
		  
	(set! contract-violations
	      (cons (make-contract-violation obj contract message srcloc blame)
		    contract-violations)))
      (inner (void) contract-failed obj contract message))

    (define/public (failed-contracts) (reverse contract-violations))
    
    (inherit add-check-failure)
    (define/pubment (property-failed result src-info)
      (add-check-failure (make-property-fail src-info (test-format) result) #f))

    (define/pubment (property-error exn src-info)
      (add-check-failure (make-property-error src-info (test-format) (exn-message exn) exn) exn))

    (super-instantiate ())))
