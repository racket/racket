#lang racket/base
(require (only-in '#%unsafe
                  unsafe-poller
                  unsafe-poll-fd
                  unsafe-poll-ctx-fd-wakeup
                  unsafe-poll-ctx-eventmask-wakeup
                  unsafe-poll-ctx-milliseconds-wakeup
                  unsafe-signal-received
                  unsafe-set-sleep-in-thread!))
(provide (all-from-out '#%unsafe))
