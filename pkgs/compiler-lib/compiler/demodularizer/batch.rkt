#lang racket/base
(require racket/cmdline
         racket/set
         raco/command-name
         "main.rkt")

(let ([output-file (make-parameter #f)])
  (command-line #:program (short-program+command-name)
                #:multi
                [("-e" "--exclude-modules") path "Exclude <path> from flattening"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                #:once-each
                [("-o") dest-filename "Write output as <dest-filename>"
                 (output-file (string->path dest-filename))]
                [("-g" "--garbage-collect") "Garbage-collect final module (unsound)"
                 (garbage-collect-toplevels-enabled #t)]                         
                [("-r" "--recompile") "Recompile final module to re-run optimizations"
                 (recompile-enabled #t)]                         
                #:args (filename)
                (unless (eq? 'racket (system-type 'vm))
                  (raise-user-error (format "~a: supported only on Racket BC"
                                            (short-program+command-name))))
                (demodularize filename (output-file))))

(module test racket/base)
