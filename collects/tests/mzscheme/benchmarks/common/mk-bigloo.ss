
(require mzlib/process)

(define name (vector-ref (current-command-line-arguments) 0))

(with-output-to-file (format "~a.scm" name)
  (lambda ()
    (write `(module fft
              (include "bigloo-prelude.sch")
              (include ,(format "~a.sch" name))))
    (newline))
  #:exists 'truncate/replace)

(when (system (format "bigloo -w -o ~a -copt -m32 -call/cc -copt -O3 -copt -fomit-frame-pointer -O6 ~a~a.scm"
                      name 
                      (if (memq (string->symbol name)
                                '(ctak))
                          "-call/cc "
                          "")
                      name))
  (delete-file (format "~a.scm" name))
  (delete-file (format "~a.o" name)))
