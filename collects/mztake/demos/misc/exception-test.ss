(require (lib "mztake.ss" "mztake"))

; tests catching of anonymously threaded exceptions

exceptions

(debug-process p ("exception.ss"))

(printf-b "exception.ss exited? ~a" (process:exited? p))
;; Prints out a behavior that tells you whether the debug-process is still running...

(process:exceptions p)
(start/resume p)