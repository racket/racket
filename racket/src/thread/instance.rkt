#lang racket/base
(require "evt.rkt"
         "sync.rkt"
         "semaphore.rkt"
         "channel.rkt"
         "schedule-info.rkt"
         "sandman.rkt"
         "atomic.rkt"
         "custodian.rkt"
         "plumber.rkt"
         "thread.rkt"
         "unsafe.rkt"
         "time.rkt"
         "place-message.rkt"
         "pre-poll.rkt")

;; Unsafe scheduler-cooperation functions are made available to
;; clients through a `#%thread` primitive linklet instance:

(provide #%thread-instance)

(define #%thread-instance
  (hasheq 'thread thread
          'thread-suspend-evt thread-suspend-evt
          'thread-dead-evt thread-dead-evt
          'current-thread current-thread
          'thread-resume thread-resume
          'make-semaphore make-semaphore
          'semaphore-post semaphore-post
          'semaphore-wait semaphore-wait
          'semaphore-peek-evt semaphore-peek-evt
          'make-channel make-channel
          'channel-put-evt channel-put-evt
          'wrap-evt wrap-evt
          'handle-evt handle-evt
          'always-evt always-evt
          'choice-evt choice-evt
          'sync sync
          'sync/timeout sync/timeout
          'evt? evt?
          'sync-atomic-poll-evt? sync-atomic-poll-evt?
          'prop:evt prop:evt
          'prop:secondary-evt prop:secondary-evt
          'poller poller
          'poller-evt poller-evt
          'poll-ctx-poll? poll-ctx-poll?
          'poll-ctx-select-proc poll-ctx-select-proc
          'poll-ctx-sched-info poll-ctx-sched-info
          'set-poll-ctx-incomplete?! set-poll-ctx-incomplete?!
          'control-state-evt control-state-evt
          'async-evt async-evt
          'current-sandman current-sandman
          'schedule-info-current-exts schedule-info-current-exts
          'schedule-info-did-work! schedule-info-did-work!
          'unsafe-start-atomic unsafe-start-atomic
          'unsafe-end-atomic unsafe-end-atomic
          'start-atomic/no-interrupts start-atomic/no-interrupts
          'end-atomic/no-interrupts end-atomic/no-interrupts
          'in-atomic-mode? in-atomic-mode?
          'current-custodian current-custodian
          'custodian-shut-down? custodian-shut-down?
          'current-plumber current-plumber
          'plumber-add-flush! plumber-add-flush!
          'plumber-flush-handle-remove! plumber-flush-handle-remove!
          'unsafe-custodian-register unsafe-custodian-register
          'unsafe-custodian-unregister unsafe-custodian-unregister
          'thread-push-kill-callback! thread-push-kill-callback!
          'thread-pop-kill-callback! thread-pop-kill-callback!
          'unsafe-add-pre-poll-callback! unsafe-add-pre-poll-callback!
          'set-get-subprocesses-time! set-get-subprocesses-time!
          'prop:place-message prop:place-message))
