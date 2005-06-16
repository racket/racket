(set-main! "exception.ss")

(printf-b "exception.ss exited? ~a" (process:exited?))

(printf-b "last exception seen: ~a" (hold (process:exceptions)))

(set-running! true)