
(require (lib "process.ss"))

(define name (vector-ref (current-command-line-arguments) 0))

(when (system (format "gsc -prelude '(include \"gambit-prelude.sch\")' ~a.sch"
               name))
 (when (system (format "gcc -o ~a -O2 -D___SINGLE_HOST ~a.c ~a_.c -lgambc -lm -ldl~a"
                name name name
                (if (file-exists? "/usr/lib/libtuil.a")
                    " -lutil" 
                    "")))
   (delete-file (format "~a.c" name))  
   (delete-file (format "~a_.c" name))))
