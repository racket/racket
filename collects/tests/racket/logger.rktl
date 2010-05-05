
(load-relative "loadtest.rktl")

(Section 'logger)

; --------------------

(test #t logger? (current-logger))
(test #f logger? 17)
(test #f logger? (make-log-receiver (current-logger) 'error))
(test #t log-receiver? (make-log-receiver (current-logger) 'error))
(test #f log-receiver? (current-logger))

(test #f logger-name (make-logger))

(arity-test make-logger 0 2)

; --------------------

(let ([l (make-logger 'test)]
      [test-level (lambda (on? l level . lrs)
                    (test on? log-level? l level)
                    (for-each (lambda (lr)
                                (test #f sync/timeout 0 lr))
                              lrs)
                    (log-message l level "message" 'data)
                    (for-each (lambda (lr)
                                (test (and on?
                                           (vector level (format "~a: message" (logger-name l)) 'data))
                                      sync/timeout 0 lr))
                              lrs))])
  (test #t logger? l)
  (test 'test logger-name l)
  (test-level #f l 'fatal)
  (test-level #f l 'error)
  (test-level #f l 'warning)
  (test-level #f l 'info)
  (test-level #f l 'debug)
  (let ([lr (make-log-receiver l 'warning)])
    (test-level #t l 'fatal lr)
    (test-level #t l 'error lr)
    (test-level #t l 'warning lr)
    (test-level #f l 'info lr)
    (test-level #f l 'debug lr)
    (let ([sub-l (make-logger 'test.sub l)])
      (test 'test logger-name l)
      (test 'test.sub logger-name sub-l)
      (test-level #t l 'fatal lr)
      (test-level #t l 'error lr)
      (test-level #t l 'warning lr)
      (test-level #f l 'info lr)
      (test-level #f l 'debug lr)
      (test-level #t sub-l 'fatal lr)
      (test-level #t sub-l 'error lr)
      (test-level #t sub-l 'warning lr)
      (test-level #f sub-l 'info lr)
      (test-level #f sub-l 'debug lr)
      (let ([lr2 (make-log-receiver sub-l 'info)])
        (test-level #t l 'fatal lr)
        (test-level #t l 'error lr)
        (test-level #t l 'warning lr)
        (test-level #f l 'info lr)
        (test-level #f l 'debug lr)
        (test-level #t sub-l 'fatal lr lr2)
        (test-level #t sub-l 'error lr lr2)
        (test-level #t sub-l 'warning lr lr2)
        (test-level #t sub-l 'info lr2)
        (test-level #f sub-l 'debug lr lr2)
        ;; Make sure they're not GCed before here:
        (list lr lr2)))))

; --------------------

(report-errs)
