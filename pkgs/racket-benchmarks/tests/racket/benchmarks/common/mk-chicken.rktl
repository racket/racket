(require racket/system)

(define name (vector-ref (current-command-line-arguments) 0))
(define exe (vector-ref (current-command-line-arguments) 1))

(system* exe
         "-no-warnings"
         "-no-trace"
         "-no-lambda-info"
         "-optimize-level" "3"
         "-block" "-d0"
         (format "~a.sch" name))
