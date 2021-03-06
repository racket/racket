#lang racket/base
(require racket/cmdline
         racket/set
         raco/command-name
         "main.rkt")

(let ([output-file (make-parameter #f)])
  (command-line #:program (short-program+command-name)
                #:once-each
                [("-o") dest-filename "Write output as <dest-filename>"
                 (output-file (string->path dest-filename))]
                #:multi
                [("-e" "--exclude-modules") path "Exclude <path> from flattening"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                #:once-each
                [("-M" "--compile-any") "Keep in machine-independent form instead of recompiling"
                 (recompile-enabled #f)]
                [("-r" "--recompile") "Recompile final module to re-run optimizations"
                 (recompile-enabled #t)]
                [("--work") dir "Cache intermediate compilations in <dir>"
                 (unless (path-string? dir)
                   (raise-user-error (format "~a: invalid work directory: ~a"
                                             (short-program+command-name)
                                             dir)))
                 (current-work-directory dir)]
                [("-g" "--garbage-collect") "Garbage-collect final module (unsound)"
                 (garbage-collect-toplevels-enabled #t)]                         
                #:args (filename)
                (demodularize filename (output-file))))

(module test racket/base)
