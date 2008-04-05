#lang scheme/base

(require scheme/class
         "test-info.scm"
         "test-display.scm")

(define test-engine%
  (class* object% ()
    (field [test-info #f]
           [test-display #f])

    (define display-class test-display%)
    (define display-rep #f)
    (define display-event-space #f)

    (super-instantiate ())

    (define/public (refine-display-class d) (set! display-class d))
    (define/public (info-class) test-info-base%)

    (define/public (add-analysis a) (send test-info add-analysis a))

    (define/public (setup-info style)
      (set! test-info (make-object (send this info-class) style)))
    (define/pubment (setup-display cur-rep event-space)
      (set! test-display (make-object display-class cur-rep))
      (set! display-rep cur-rep)
      (set! display-event-space event-space)
      (inner (void) setup-display cur-rep event-space))

    (define/pubment (run)
      (unless test-info (send this setup-info 'check-base))
      (inner (void) run))
    (define/public (summarize-results port)
      (unless test-display (setup-display #f #f))
      (let ([result (send test-info summarize-results)])
        (case result
          [(no-tests) (send this display-untested port)]
          [(all-passed) (send this display-success port)]
          [(mixed-results)
           (send this display-results display-rep display-event-space)])))

    (define/public (display-success port)
      (fprintf port "All tests passed!~n"))
    (define/public (display-untested port)
      (fprintf port "This program should be tested.~n"))
    (define/public (display-results rep event-space)
      (send test-display install-info test-info)
      (if event-space
        (parameterize ([(dynamic-require 'scheme/gui 'current-eventspace)
                        event-space])
          ((dynamic-require 'scheme/gui 'queue-callback)
           (lambda () (send rep display-test-results test-display))))
        (send test-display display-results)))

    (define/pubment (initialize-test test)
      (inner (void) initialize-test test))

    (define/pubment (run-test test)
      (inner (void) run-test test))

    (define/pubment (run-testcase testcase)
      (inner (void) run-testcase testcase))))

(provide test-engine%)
