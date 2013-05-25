#lang racket/base
(require "pool.rkt"
         "queue.rkt")

(define pump-thread (cocoa-start-event-pump))
(cocoa-install-event-wakeup)
