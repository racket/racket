
(require mzlib/process)

(define name (vector-ref (current-command-line-arguments) 0))

(when (file-exists? (format "~a.o1" name))
  (delete-file (format "~a.o1" name)))

(system (format "gsc -:m10000~a -dynamic -prelude '(include \"gambit-prelude.sch\")' ~a.sch"
                (if (memq (string->symbol name) '(nucleic2))
                    ",s"
                    "")
                name))
