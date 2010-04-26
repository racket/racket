#lang scheme/base
(require scheme/cmdline
         raco/command-name
         compiler/zo-parse
         compiler/decompile
         scheme/pretty)

(define source-files
  (command-line
   #:program (short-program+command-name)
   #:args source-or-bytecode-file
   source-or-bytecode-file))

(for ([zo-file source-files])
  (let ([zo-file (path->complete-path zo-file)])
    (let-values ([(base name dir?) (split-path zo-file)])
      (let ([alt-file (build-path base "compiled" (path-add-suffix name #".zo"))])
        (parameterize ([current-load-relative-directory base]
                       [print-graph #t])
          (pretty-print
           (decompile
            (call-with-input-file*
             (if (file-exists? alt-file) alt-file zo-file)
             (lambda (in)
               (zo-parse in))))))))))
