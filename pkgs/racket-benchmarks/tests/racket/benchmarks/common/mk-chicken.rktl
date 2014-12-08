(require mzlib/process)

(define name (vector-ref (current-command-line-arguments) 0))

(system (format "csc -no-warnings -no-trace -no-lambda-info -optimize-level 3 -block -d0 ~a.sch"
                name))
