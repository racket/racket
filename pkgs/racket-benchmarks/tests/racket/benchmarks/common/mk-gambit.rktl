(require racket/system)

(define name (vector-ref (current-command-line-arguments) 0))
(define exe (vector-ref (current-command-line-arguments) 1))

(when (file-exists? (format "~a.o1" name))
  (delete-file (format "~a.o1" name)))

(system* exe
         (format "-:m10000~a"
                 (if (memq (string->symbol name) '(nucleic2))
                     ",s"
                     ""))
         "-dynamic"
         "-prelude" "(include \"gambit-prelude.sch\")"
         (format "~a.sch" name))
