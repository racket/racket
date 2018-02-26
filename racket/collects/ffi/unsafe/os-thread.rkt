#lang racket/base
(require '#%unsafe)

(provide
 (rename-out [unsafe-os-thread-enabled? os-thread-enabled?]
             [unsafe-call-in-os-thread call-in-os-thread]
             [unsafe-make-os-semaphore make-os-semaphore]
             [unsafe-os-semaphore-post os-semaphore-post]
             [unsafe-os-semaphore-wait os-semaphore-wait]))
