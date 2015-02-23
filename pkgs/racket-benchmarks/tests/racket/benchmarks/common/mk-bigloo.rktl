
(require mzlib/process)

(define name (vector-ref (current-command-line-arguments) 0))

(with-output-to-file (format "~a.scm" name)
  (lambda ()
    (write `(module fft
              (include "bigloo-prelude.sch")
              (include ,(format "~a.sch" name))))
    (newline))
  #:exists 'truncate/replace)

(when (system (format "bigloo -static-bigloo -w -o ~a -call/cc -copt -O3 -copt -fomit-frame-pointer -O6 ~a.scm"
                      name name))
  (delete-file (format "~a.scm" name))
  (delete-file (format "~a.o" name)))
