#lang scheme/base
(require scheme/cmdline
         raco/command-name
         scheme/pretty)

(define source-files
  (command-line
   #:program (short-program+command-name)
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
                 (pretty-print (syntax->datum (expand e)))
                 (loop))))))))))
