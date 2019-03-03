
(load-relative "loadtest.rktl")

(Section 'logger)

; --------------------

(test #t logger? (current-logger))
(test #f logger? 17)
(test #f logger? (make-log-receiver (current-logger) 'error))
(test #t log-receiver? (make-log-receiver (current-logger) 'error))
(test #f log-receiver? (current-logger))

(test #f logger-name (make-logger))

(arity-test make-logger 0 -1)

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
                              lrs)
                    (log-message l level 'name "message" 'data #f)
                    (for-each (lambda (lr)
                                (test (and on?
                                           (vector level "message" 'data 'name))
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

(parameterize ([current-logger (make-logger)])
  (define-logger test)
  (test #t logger? test-logger)
  (define r (make-log-receiver (current-logger) 'warning 'test))
  (log-test-debug (/ 0))
  (log-test-debug "debug")
  (test #f sync/timeout 0 r)
  (log-test-warning "warning")
  (test '(#f #f warning test) log-all-levels test-logger)
  (test "test: warning" (lambda (v) (vector-ref v 1)) (sync r)))

(parameterize ([current-logger (make-logger)])
  (define-logger test)
  (define r (make-log-receiver (current-logger) 'info 'test2 'warning))
  (test #t log-level? test-logger 'warning)
  (test #t log-level? test-logger 'info)
  (test #t log-level? test-logger 'info 'test2)
  (test #f log-level? test-logger 'info 'not-test)
  (test #f log-level? test-logger 'debug 'test2)
  (test 'info log-max-level test-logger)
  (test 'info log-max-level test-logger 'test2)
  (test 'warning log-max-level test-logger 'not-test)
  (test '(warning #f info test2) log-all-levels test-logger)
  ;; Retain receiver to avoid GC influence on tests
  (test #f sync/timeout 0 r))

(parameterize ([current-logger (make-logger)])
  (define-logger test)
  (define r2 (make-log-receiver (current-logger) 'warning 'test3 'info))
  (test #f log-level? test-logger 'info 'test3)
  (test #t log-level? test-logger 'info 'not-test)
  (test #f log-level? test-logger 'debug 'test3)
  (test 'info log-max-level test-logger)
  (test 'warning log-max-level test-logger 'test3)
  (test 'info log-max-level test-logger 'not-test)
  ;; Retain receiver to avoid GC influence on tests
  (test #f sync/timeout 0 r2))

; ----------------------------------------

(let ()
  (define root (make-logger))
  (define sub1 (make-logger 'sub1 root))
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

(let ()
  (define l (make-logger))
  (define r (make-log-receiver l 'info 'sub))
  (log-message l 'info 'sub "hey" #f)
  (test '#(info "sub: hey" #f sub) sync/timeout 0 r))

;; --------------------
;; Check logger propagate constraints

(let ()
  (define l (make-logger))
  (define l2 (make-logger #f l 'error))
  (define l3 (make-logger #f l2 'warning 'test 'info))
  (define l32 (make-logger #f l2 'info 'test 'warning))
  
  (define evt (log-level-evt l32))
  (test #f sync/timeout 0 evt)
  
  (define r (make-log-receiver l 'debug))
  (test #f sync/timeout 0 r)
  (test evt sync/timeout 0 evt)
  
  (log-message l 'debug "debug message" #f)
  (test #t vector? (sync/timeout 0 r))
  
  (define r2 (make-log-receiver l2 'info))
  (test (void) log-message l2 'warning "warning message not propagated" #f)
  (test #f sync/timeout 0 r)
  (test #t vector? (sync/timeout 0 r2))

  (test (void) log-message l3 'error "error message propagated" #f)
  (test #t vector? (sync/timeout 0 r))
  (test #t vector? (sync/timeout 0 r2))

  (test (void) log-message l3 'info "info message partially propagated" #f)
  (test #f sync/timeout 0 r)
  (test #t vector? (sync/timeout 0 r2))
  
  (test (void) log-message l3 'info 'test "info message not propagated" #f)
  (test #f sync/timeout 0 r)
  (test #f sync/timeout 0 r2)
  
  (test 'debug log-max-level l)
  (test 'info log-max-level l2)
  (test 'info log-max-level l3)

  (test '(debug #f) log-all-levels l)
  (test '(info #f) log-all-levels l2)

  (define r22 (make-log-receiver l2 'debug))
  (test 'debug log-max-level l)
  (test 'debug log-max-level l2)
  (test 'info log-max-level l3)
  (test 'warning log-max-level l3 'test)
  (test 'info log-max-level l3 'not-test)
  (test 'info log-max-level l32)
  (test 'warning log-max-level l32 'not-test)
  (test 'info log-max-level l32 'test)

  (test '(debug #f) log-all-levels l)
  (test '(debug #f) log-all-levels l2)

  ;; Retain receivers to avoid GC influence on tests
  (test #f sync/timeout 0 r)
  (test #f sync/timeout 0 r2)
  (test #f sync/timeout 0 r22))

; --------------------
; racket/logging

(require racket/logging)
(define (test-intercepted-logging)
  (define log '())
  (cons (with-intercepted-logging
            (lambda (v) (set! log (cons (vector-ref v 1) log)))
          (lambda ()
            (log-warning "1")
            (log-warning "2")
            (log-warning "3")
            (log-info "4")
            #t)
          'warning)
        log))
(test '(#t "3" "2" "1") test-intercepted-logging)

;; From issue #1486
(define (test-intercepted-logging2)
  (let ([warning-counter 0]
        [l (current-logger)])
    (with-intercepted-logging
        #:logger l
        (lambda (l)
          (when (eq? (vector-ref l 0)
                     'warning)
            (set! warning-counter (add1 warning-counter))))
      (lambda ()
        (log-message l 'warning "Warning!" (current-continuation-marks))
        (log-message l 'warning "Warning again!" (current-continuation-marks))
        (+ 2 2))
      'warning)
    warning-counter))
(test 2 test-intercepted-logging2)

; --------------------
;; Check that a blocked log receiver is not GCed if
;; if might receiver something

(let ()
  (define s (make-semaphore))
  (let ([lr (make-log-receiver (current-logger)
                               'info)])
    (thread (lambda ()
              (semaphore-post s))))
  (sync (system-idle-evt))
  (collect-garbage)
  (log-message (current-logger) 'info  "" 'c)
  ;; If receiver is GCed, then this will block
  (sync s))

; --------------------
; `define-logger` with explicit parent

(let ()
  (define-logger parent)
  (define-logger child #:parent parent-logger)
  (define r (make-log-receiver parent-logger 'warning 'child))
  (log-child-debug "debug")
  (test #f sync/timeout 0 r)
  (log-child-warning "warning")
  (test "child: warning" (lambda (v) (vector-ref v 1)) (sync r)))

(let ()
  (define-logger parent)
  (define parent-receiver (make-log-receiver parent-logger 'warning 'no-parent))
  (parameterize ([current-logger parent-logger])
    (define-logger no-parent #:parent #f)
    (define no-parent-receiver (make-log-receiver no-parent-logger 'warning 'no-parent))
    (log-no-parent-warning "warning")
    (test #f sync/timeout 0 parent-receiver)
    (test "no-parent: warning" (lambda (v) (vector-ref v 1)) (sync no-parent-receiver))))

(err/rt-test
 (let ()
   (define-logger test #:parent 'not-a-logger)
   (void))
 exn:fail:contract?
 #rx"define-logger: contract violation.+expected: \\(or/c logger\\? #f\\).+given: 'not-a-logger")

; --------------------
;; optional data for `log-message`

(let ()
  (define logger (make-logger))
  (define m (make-log-receiver logger 'error))
  (log-message logger 'error 'whatever "hi")
  (test '#(error "whatever: hi" #f whatever) sync m))

(let ()
  (define logger (make-logger))
  (define m (make-log-receiver logger 'error))
  (log-message logger 'error "hi")
  (test '#(error "hi" #f #f) sync m))

; --------------------

(report-errs)
