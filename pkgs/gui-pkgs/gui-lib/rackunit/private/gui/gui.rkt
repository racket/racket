#lang racket/base
(require racket/class
         unstable/class-iop
         rackunit/private/base
         rackunit/private/result
         rackunit/private/check-info
         "interfaces.rkt"
         "controller.rkt"
         "view.rkt")
(provide make-gui-runner)

(define (make-gui-runner)
  (define controller
    (new controller%))
  (define frame
    (make-view-frame controller))
  (lambda tests
    (for ([test (in-list tests)])
      (run test controller))))

(define (run test controller)
  ;; state = parent result<%>

  (define (for-suite-entry suite name before after state)
    (define model
      (send/i controller controller<%> create-model suite state))
    (before)
    model)

  (define (for-suite-exit suite name before after state kid-state)
    (after)
    (send/i kid-state suite<%> finish!)
    state)

  (define (for-case case name action state)
    (define model
      (send/i controller controller<%> create-model case state))
    (run-case case model)
    state)

  (foldts-test-suite for-suite-entry for-suite-exit for-case
                     #f test))

;; From old suite-runner:
#|
(define/public (run)
  (let ([custodian (make-custodian)]
        [before (rackunit-test-suite-before test)]
        [after (rackunit-test-suite-after test)])
    (parameterize [(current-custodian custodian)]
      (dynamic-wind
        before
        (lambda ()
          (for-each (lambda (c) (send c run)) (get-children))
          (custodian-shutdown-all custodian))
        after)))
  (on-child-status-change #f))
|#

;; ----

(define (run-case test model)
  (define primerr (current-error-port))
  (define iport (open-input-string ""))
  (define super-cust (current-custodian))
  (define cust (make-custodian))
  (define-values (oport errport get-output)
    (make-output-ports))
  (let-values ([(test-result timing)
                (parameterize [(current-input-port iport)
                               (current-output-port oport)
                               (current-error-port errport)
                               (current-custodian cust)]
                  (run/time-test test))])
    ;;(set! timing times)
    (define trash
      (map (lambda (x) (format "~s" x))
           (custodian-managed-list cust super-cust)))
    (cond [(test-success? test-result)
           (send/i model case<%> update
                   test-result
                   (test-success-result test-result)
                   null
                   timing
                   (get-output)
                   trash)]
          [(test-failure? test-result)
           (let* ([exn (test-failure-result test-result)]
                  [property-stack (exn:test:check-stack exn)])
             (send/i model case<%> update
                     test-result
                     (test-failure-result test-result)
                     (for/list ([pp property-stack])
                       (cons (check-info-name pp) (check-info-value pp)))
                     timing
                     (get-output)
                     trash))]
          [(test-error? test-result)
           (send/i model case<%> update
                   test-result
                   (test-error-result test-result)
                   null
                   timing
                   (get-output)
                   trash)])))

(define (run/time-test test)
  (let-values ([(results cputime realtime gctime)
                (call-with-continuation-prompt
                 (lambda ()
                   (time-apply run-test-case 
                               (list (rackunit-test-case-name test)
                                     (rackunit-test-case-action test)))))])
    (values (car results) (list cputime realtime gctime))))

(define (make-output-ports)
  (define output null)
  (define output-sema (make-semaphore 1))
  (define (make-output-collector tag)
    (define (do-write-out buf start end)
      (define subbuf (subbytes buf start end))
      (if (and (pair? output)
               (eq? (car (car output)) tag))
          ;; Coalesce
          (let ([prev (cdr (car output))])
            (set! output
                  (cons (cons tag (cons subbuf prev)) (cdr output))))
          (set! output (cons (list tag subbuf) output)))
      (- end start))
    (define name #f)
    (define evt output-sema)
    (define (write-out buf start end buffer? enable-break?)
      ((if enable-break? sync/enable-break sync) output-sema)
      (begin0 (do-write-out buf start end) (semaphore-post output-sema)))
    (define (close) (void))
    (define (get-write-evt buf start end)
      (wrap-evt output-sema
                (lambda (_)
                  (begin0 (write-out buf start end #f #f)
                    (semaphore-post output-sema)))))
    (make-output-port name evt write-out close #f
                      get-write-evt #f))
  (values (make-output-collector 'output)
          (make-output-collector 'error)
          (lambda () output)))
