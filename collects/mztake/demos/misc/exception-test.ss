#| This program starts a thread, the thread raises an exception,
   this tests how MzTake catches exceptions, even if they come from
   anonymous locations.

   We don't even need to bind any variables or add any breaks, we just
   run the program and catch the exception it throws. |#

(define-mztake-process p ("exception.ss"))

(printf-b "exception.ss exited? ~a" (process:exited? p))
#| Prints out a behavior that tells you whether the debug-process is still running... |#

(printf-b "last exception seen: ~a" (hold (process:exceptions p)))
#| Prints out the last exception that the program threw |#

(start/resume p)