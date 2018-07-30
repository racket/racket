#lang racket/base

(module expand racket/base
  (require racket/cmdline
           raco/command-name
           racket/pretty)
  
  (provide show-program)

  (define print-only (make-parameter #f))

  (define (show-program expand)
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
       ["-p" "Print the document back out without performing expansion." (print-only #t)]
       #:args source-file
       source-file))

    (for ([src-file source-files])
      (let ([src-file (path->complete-path src-file)]
            [f (if (print-only) (lambda (i) i) expand)])
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
                     (pretty-write (syntax->datum (f e)))
                     (loop))))))))))))

(require (submod "." expand))
(show-program expand)
  
