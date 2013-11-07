#lang racket/base
(require racket/cmdline
         raco/command-name
         compiler/zo-parse
         compiler/decompile
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
   #:args source-or-bytecode-file
   source-or-bytecode-file))

(for ([zo-file source-files])
  (let ([zo-file (path->complete-path zo-file)])
    (let-values ([(base name dir?) (split-path zo-file)])
      (let ([alt-file (build-path base "compiled" (path-add-suffix name #".zo"))])
        (parameterize ([current-load-relative-directory base]
                       [print-graph #t])
          (pretty-write
           (decompile
            (call-with-input-file*
             (if (file-exists? alt-file) alt-file zo-file)
             (lambda (in)
               (zo-parse in))))))))))
