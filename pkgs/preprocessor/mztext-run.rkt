#lang scheme/base

(require preprocessor/mztext preprocessor/pp-run scheme/cmdline)

(let ([output #f] [run-cmd #f])
  (command-line
   #:once-each
   [("-o" "--output") file "output file (defaults to standard output)"
    (set! output file)]
   [("-c" "--command-marker") marker "command marker (defaults to \"@\")"
    (command-marker marker)]
   [("--run") cmd "run the command string on a single preprocessed input file"
    (set! run-cmd cmd)]
   #:help-labels "   (see the documentation for this option)"
   #:multi
   [("-E" "--eval") expr "evaluates <expr> before processing starts"
    (parameterize ([read-case-sensitive #t])
      (add-eval (read-syntax 'command-line (open-input-string expr) )))]
   #:handlers
   (lambda (_ . files) (run preprocess run-cmd output files))
   '("input-file")
   (more-help 'mztext "a Racket-based preprocessing language")))
