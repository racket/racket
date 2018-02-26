#lang racket/base
(require "check.rkt"
         "evt.rkt"
         "schedule-info.rkt")

(provide (rename-out [create-alarm-evt alarm-evt]))

(struct alarm-evt (msecs)
  #:property
  prop:evt
  (poller (lambda (e ctx)
            (define msecs (alarm-evt-msecs e))
            (if ((current-inexact-milliseconds) . >= . msecs)
                (values (list e) #f)
                (begin
                  (schedule-info-add-timeout-at! (poll-ctx-sched-info ctx)
                                                 msecs)
                  (values #f e))))))

(define/who (create-alarm-evt msecs)
  (check who real? msecs)
  (alarm-evt msecs))
