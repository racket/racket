(define-mztake-process p ("exception.ss"))

(printf-b "exception.ss exited? ~a" (process:exited? p))

(printf-b "last exception seen: ~a" (hold (process:exceptions p)))

(start/resume p)