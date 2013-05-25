
(load-relative "loadtest.rktl")

(Section 'logger)

; --------------------

(test #t logger? (current-logger))
(test #f logger? 17)
(test #f logger? (make-log-receiver (current-logger) 'error))
(test #t log-receiver? (make-log-receiver (current-logger) 'error))
(test #f log-receiver? (current-logger))

(test #f logger-name (make-logger))

(arity-test make-logger 0 3)

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
                                           (vector level (format "~a: message" (logger-name l)) 'data (logger-name l)))
                                      sync/timeout 0 lr))
                              lrs)
                    (log-message l level 'name "message" 'data)
                    (for-each (lambda (lr)
                                (test (and on?
                                           (vector level "name: message" 'data 'name))
                                      sync/timeout 0 lr))
                              lrs))])
  (test #t logger? l)
  (test 'test logger-name l)
  (test #f log-max-level l)
  (test-level #f l 'fatal)
  (test-level #f l 'error)
  (test-level #f l 'warning)
  (test-level #f l 'info)
  (test-level #f l 'debug)
  (let ([lr (make-log-receiver l 'warning)])
    (test 'warning log-max-level l)
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
      (test 'warning log-max-level sub-l)
      (let ([lr2 (make-log-receiver sub-l 'info)])
        (test 'info log-max-level sub-l)
        (test 'warning log-max-level l)
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

(let ()
  (define-logger test)
  (test #t logger? test-logger)
  (define r (make-log-receiver (current-logger) 'warning 'test))
  (log-test-debug (/ 0))
  (log-test-debug "debug")
  (test #f sync/timeout 0 r)
  (log-test-warning "warning")
  (test "test: warning" (lambda (v) (vector-ref v 1)) (sync r)))

; ----------------------------------------

(let ()
  (define root (make-logger))
  (define sub1 (make-logger 'sub1 root #f))
  (define sub2 (make-logger 'sub2 root))
  (define sub3 (make-logger 'sub3 root))
  (define sub4 (make-logger 'sub4 root))
  (define r (make-log-receiver root 
                               'error #f
                               'info 'sub1 
                               'none 'sub4
                               'info 'sub2
                               'warning 'sub1 
                               'fatal #f))
  (define (get)
    (define m (sync/timeout 0 r))
    (and m (vector-ref m 1)))
  (log-message root 'debug "message" 'data)
  (test #f get)
  (log-message sub1 'info "message" 'data)
  (test #f get)
  (log-message sub2 'info "message" 'data)
  (test "sub2: message" get)
  (log-message sub2 'debug "message" 'data)
  (test #f get)
  (log-message sub1 'warning "message" 'data)
  (test "sub1: message" get)
  (log-message root 'fatal "message" 'data)
  (test "message" get)
  (log-message sub3 'fatal "message" 'data)
  (test "sub3: message" get)
  (log-message sub3 'debug "message" 'data)
  (log-message sub3 'info "message" 'data)
  (log-message sub3 'warning "message" 'data)
  (test #f get)
  (log-message sub4 'debug "message" 'data)
  (log-message sub4 'info "message" 'data)
  (log-message sub4 'warning "message" 'data)
  (log-message sub4 'error "message" 'data)
  (log-message sub4 'fatal "message" 'data)
  (test #f get))

; --------------------
;; notification callback:

(let ()
  (define rt #f)
  (define s1 #f)
  (define root (make-logger #f #f (lambda (m) (set! rt m))))
  (define sub1 (make-logger #f root (lambda (m) (set! s1 m))))
  ;; no receivers:
  (log-message sub1 'debug "message" 'data)
  (test #f values rt)
  (test #f values s1)
  (define r (make-log-receiver root 'error))
  ;; still no receivers for 'debug:
  (log-message root 'debug "message" 'data)
  (test #f values rt)
  (test #f values s1)
  ;; receivers for 'error:
  (log-message sub1 'error "message" 'data)
  (test rt vector 'error "message" 'data #f)
  (test s1 vector 'error "message" 'data #f)
  (set! rt #f)
  (set! s1 #f)
  (log-message root 'fatal 'name "message2" 'data2)
  (test rt vector 'fatal "name: message2" 'data2 'name)
  (test #f values s1)
  (define sub2 (make-logger 'sub2 root (lambda (m) (abort-current-continuation 
                                                    (default-continuation-prompt-tag) 
                                                    void))))
  (test 'aborted
        call-with-continuation-prompt
        (lambda () (log-message sub2 'fatal 'name "message2" 'data2))
        (default-continuation-prompt-tag) 
        (lambda (v) 'aborted))
  
  (void))

; --------------------

(report-errs)
