
(define-primitive-table futures-table
  [current-future (known-procedure 1)]
  [fsemaphore-count (known-procedure 2)]
  [fsemaphore-post (known-procedure 2)]
  [fsemaphore-try-wait? (known-procedure 2)]
  [fsemaphore-wait (known-procedure 2)]
  [fsemaphore? (known-procedure 2)]
  [future (known-procedure 2)]
  [future? (known-procedure 2)]
  [futures-enabled? (known-procedure 1)]
  [make-fsemaphore (known-procedure 2)]
  [mark-future-trace-end! (known-procedure 1)]
  [processor-count (known-procedure 1)]
  [reset-future-logs-for-tracing! (known-procedure 1)]
  [touch (known-procedure 2)]
  [would-be-future (known-procedure 2)])
