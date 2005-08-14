(require (lib "mztake.ss" "mztake")
         (lib "useful-code.ss" "mztake"))

(set-main! "exception.ss")

(printf-b "exception.ss exited? ~a" (exited?))

(printf-b "last exception seen: ~a" (exceptions))

(set-running! true)