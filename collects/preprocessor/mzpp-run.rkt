#lang scheme/base

(require preprocessor/mzpp preprocessor/pp-run scheme/cmdline)

(let ([output #f] [run-cmd #f])
  (command-line
   #:once-each
   [("-o" "--output") file "output file (defaults to standard output)"
    (set! output file)]
   [("-b" "--begin-mark") beg "Scheme beginning marker (defaults to \"<<\")"
    (beg-mark beg)]
   [("-e" "--end-mark") end "Scheme ending marker (defaults to \">>\")"
    (end-mark end)]
   [("-s" "--skip-to") skip "skip processing to a line with only this string"
    (skip-to skip)]
   [("--no-spaces") "disable \"smart\" handling of spaces"
    (no-spaces? #t)]
   [("--run") cmd "run the command string on a single preprocessed input file"
    (set! run-cmd cmd)]
   #:help-labels "   (see the documentation for this option)"
   #:multi
   [("-E" "--eval") expr "evaluates <expr> before processing starts"
    (parameterize ([read-case-sensitive #t])
      (add-eval (read-syntax 'command-line (open-input-string expr))))]
   #:once-each
   [("--debug") "show preprocessed Scheme code (for debugging)"
    (debug? #t)]
   #:handlers
   (lambda (_ . files) (run preprocess run-cmd output files))
   '("input-file")
   (more-help 'mzpp "a Racket-based preprocessor")))
