#lang racket/base
(require "check.rkt"
         "evt.rkt"
         "schedule-info.rkt")

(provide (rename-out [create-alarm-evt alarm-evt]))

(struct alarm-evt (msecs monotonic)
  #:property
  prop:evt
  (poller (lambda (e ctx)
            (define msecs (alarm-evt-msecs e))
            (define monotonic? (alarm-evt-monotonic e))
            (define current-ms (if monotonic? current-inexact-monotonic-milliseconds current-inexact-milliseconds))
            (if ((current-ms) . >= . msecs)
                (values (list e) #f)
                (begin
                  (schedule-info-add-timeout-at! (poll-ctx-sched-info ctx)
                                                 (if monotonic?
                                                     msecs
                                                     (+ (current-inexact-monotonic-milliseconds)
                                                        (- msecs (current-inexact-milliseconds)))))
                  (values #f e))))))

(define/who (create-alarm-evt msecs [monotonic? #f])
  (check who real? msecs)
  (alarm-evt msecs monotonic?))
