#lang racket/base
(require '#%futures)

(provide  future?
          future
          touch
          processor-count
          current-future
          fsemaphore?
          make-fsemaphore
          fsemaphore-count
          fsemaphore-post
          fsemaphore-wait
          fsemaphore-try-wait?
          would-be-future
          futures-enabled?)
