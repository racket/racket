
(define-primitive-table place-table
  [dynamic-place (known-procedure 32)]
  [place-break (known-procedure 6)]
  [place-channel (known-procedure 1)]
  [place-channel-get (known-procedure 2)]
  [place-channel-put (known-procedure 4)]
  [place-channel? (known-procedure 2)]
  [place-dead-evt (known-procedure 2)]
  [place-enabled? (known-procedure 1)]
  [place-kill (known-procedure 2)]
  [place-message-allowed? (known-procedure 2)]
  [place-pumper-threads (known-procedure 6)]
  [place-shared? (known-procedure 2)]
  [place-wait (known-procedure 2)]
  [place? (known-procedure 2)])
