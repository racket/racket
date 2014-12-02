#lang racket/base
(require racket/cmdline
         raco/command-name
         racket/pretty)

(define source-files
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("--columns" "-n") n "Format for <n> columns"
    (let ([num (string->number n)])
      (unless (exact-positive-integer? num)
        (raise-user-error (string->symbol (short-program+command-name))
                          "not a valid column count: ~a" n))
      (pretty-print-columns num))]
   #:args source-file
   source-file))

(for ([src-file source-files])
  (let ([src-file (path->complete-path src-file)])
    (let-values ([(base name dir?) (split-path src-file)])
      (parameterize ([current-load-relative-directory base]
                     [current-namespace (make-base-namespace)]
                     [read-accept-reader #t])
        (call-with-input-file*
         src-file
         (lambda (in)
           (port-count-lines! in)
           (let loop ()
             (let ([e (read-syntax src-file in)])
               (unless (eof-object? e)
                 (pretty-write (syntax->datum (expand e)))
                 (loop))))))))))
