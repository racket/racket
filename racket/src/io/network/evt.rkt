#lang racket/base
(require "../host/rktio.rkt"
         "../host/thread.rkt"
         "../sandman/main.rkt")

(provide rktio-evt)

(struct rktio-evt (poll add-to-poll-set)
  #:property
  prop:evt
  (poller
   (lambda (self poll-ctx)
     (cond
       [((rktio-evt-poll self))
        (values (list self) #f)]
       [else
        (define sched-info (poll-ctx-sched-info poll-ctx))
        (when sched-info
          ;; Cooperate with the sandman by registering a function that
          ;; takes a poll set and adds to it:
          (schedule-info-current-exts sched-info
                                      (sandman-add-poll-set-adder
                                       (schedule-info-current-exts sched-info)
                                       (rktio-evt-add-to-poll-set self))))
        (values #f self)])))
  #:authentic)
