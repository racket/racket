(require racket/system)

(define name (vector-ref (current-command-line-arguments) 0))
(define exe (vector-ref (current-command-line-arguments) 1))

(with-output-to-file (format "~a.scm" name)
  (lambda ()
    (write `(module fft
              (include "bigloo-prelude.sch")
              (include ,(format "~a.sch" name))))
    (newline))
  #:exists 'truncate/replace)

(when (system* exe
               "-static-bigloo"
               "-w"
               "-o" name
               "-call/cc"
               "-copt"
               "-O3"
               "-copt"
               "-fomit-frame-pointer"
               "-O6"
               (format "~a.scm" name))
  (delete-file (format "~a.scm" name))
  (delete-file (format "~a.o" name)))
