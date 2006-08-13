
(require (lib "process.ss"))

(define name (vector-ref (current-command-line-arguments) 0))

(with-output-to-file (format "~a.scm" name)
  (lambda ()
    (write `(module fft
              (include "bigloo-prelude.sch")
              (include ,(format "~a.sch" name))))
    (newline))
  'truncate/replace)

(when (system (format "bigloo -w -o ~a -O6 ~a.scm"
                      name name))
  (delete-file (format "~a.scm" name))
  (delete-file (format "~a.o" name)))
