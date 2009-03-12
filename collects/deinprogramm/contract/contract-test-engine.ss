#lang scheme/base

(provide build-contract-test-engine
	 contract-violation?
	 contract-violation-obj contract-violation-contract contract-violation-messages
	 contract-violation-blame contract-violation-srcloc)

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
    (inherit setup-info display-untested)

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
	(fprintf port "Tests disabled.\n"))))

    (define/private (display-success port event-space count)
      (clear-results event-space)
      (unless (test-silence)
        (fprintf port "~a test~a passed!\n"
                 (case count
                   [(0) "Zero"]
                   [(1) "The only"]
                   [(2) "Both"]
                   [else (format "All ~a" count)])
                 (if (= count 1) "" "s"))))

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

(define-struct contract-violation (obj contract messages srcloc blame))

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
	     (messages
	      (if message
		  (list message)
		  (list "got " ((test-format) obj)))))
		  
	(set! contract-violations
	      (cons (make-contract-violation obj contract messages srcloc blame)
		    contract-violations)))
      (inner (void) contract-failed obj contract message))

    (define/public (failed-contracts) (reverse contract-violations))

    (super-instantiate ())))
