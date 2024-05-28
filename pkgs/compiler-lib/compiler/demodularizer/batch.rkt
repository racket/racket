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
                [("-x" "--exclude-library") module-path "Exclude `(lib <module-path>)` from flattening"
                 (unless (module-path? `(lib ,module-path))
                   (raise-user-error (format "~a: invalid module path: (lib ~s)"
                                             (short-program+command-name)
                                             module-path)))
                 (define r (module-path-index-resolve (module-path-index-join `(lib ,module-path) #f)))
                 (define path (resolved-module-path-name r))
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                [("-e" "--exclude-module") path "Exclude <path> from flattening"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                [("--exclude-modules") path "Compatibility alias for `--exclude-module`"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                #:once-each
                [("-s" "--syntax") "Preserve syntax objects, macros, and provides"
                 (syntax-object-preservation-enabled #t)]
                [("--max-phase") phase "Preserve only phases through <phase>"
                 (define ph (string->number phase))
                 (unless (exact-nonnegative-integer? ph)
                   (raise-user-error (format "~a: invalid phase: ~s"
                                             (short-program+command-name)
                                             phase)))
                 (current-maximum-phase ph)]
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
                [("--dump") dest-filename "Dump S-expression form to <dest-filename>"
                 (current-merged-output-file dest-filename)]
                #:args (filename)
                (demodularize filename (output-file))))

(module test racket/base)
