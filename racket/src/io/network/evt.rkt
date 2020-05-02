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
        (sandman-poll-ctx-add-poll-set-adder! poll-ctx (rktio-evt-add-to-poll-set self))
        (values #f self)])))
  #:authentic)
