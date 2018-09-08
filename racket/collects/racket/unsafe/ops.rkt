#lang racket/base
(require '#%unsafe
         '#%flfxnum
         '#%extfl
         "../private/kw.rkt")

(provide (except-out (all-from-out '#%unsafe)
                     unsafe-undefined
                     check-not-unsafe-undefined
                     check-not-unsafe-undefined/assign
                     prop:chaperone-unsafe-undefined
                     chaperone-struct-unsafe-undefined
                     unsafe-chaperone-procedure
                     unsafe-impersonate-procedure
                     unsafe-start-atomic unsafe-end-atomic
                     unsafe-start-breakable-atomic unsafe-end-breakable-atomic
                     unsafe-in-atomic?
                     unsafe-poller
                     unsafe-poll-ctx-fd-wakeup
                     unsafe-poll-ctx-eventmask-wakeup
                     unsafe-poll-ctx-milliseconds-wakeup
                     unsafe-signal-received unsafe-set-sleep-in-thread!
                     unsafe-file-descriptor->port
                     unsafe-port->file-descriptor
                     unsafe-file-descriptor->semaphore
                     unsafe-socket->port
                     unsafe-port->socket
                     unsafe-socket->semaphore
                     unsafe-thread-at-root
                     unsafe-make-custodian-at-root
                     unsafe-custodian-register
                     unsafe-custodian-unregister
                     unsafe-add-post-custodian-shutdown
                     unsafe-register-process-global
                     unsafe-get-place-table
                     unsafe-make-security-guard-at-root
                     unsafe-set-on-atomic-timeout!
                     unsafe-abort-current-continuation/no-wind
                     unsafe-call-with-composable-continuation/no-wind
                     unsafe-root-continuation-prompt-tag
                     unsafe-add-global-finalizer
                     unsafe-os-thread-enabled?
                     unsafe-call-in-os-thread
                     unsafe-make-os-semaphore
                     unsafe-os-semaphore-post
                     unsafe-os-semaphore-wait
                     unsafe-add-collect-callbacks
                     unsafe-remove-collect-callbacks
                     unsafe-make-place-local
                     unsafe-place-local-ref
                     unsafe-place-local-set!)
         (rename-out [new:unsafe-impersonate-procedure unsafe-impersonate-procedure]
                     [new:unsafe-chaperone-procedure unsafe-chaperone-procedure])
         (prefix-out unsafe-
                     (combine-out flsin flcos fltan
                                  flasin flacos flatan
                                  fltruncate flround flfloor flceiling
                                  flexp fllog flexpt

                                  extflsin extflcos extfltan
                                  extflasin extflacos extflatan
                                  extfltruncate extflround extflfloor extflceiling
                                  extflexp extfllog extflexpt)))
