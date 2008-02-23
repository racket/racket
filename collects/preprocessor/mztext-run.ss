(module mztext-run mzscheme

(require preprocessor/mztext preprocessor/pp-run mzlib/cmdline)

(let ([output #f] [run-cmd #f])
  (command-line
   "mztext"
   (current-command-line-arguments)
   (once-each
    [("-o" "--output") file "output file (defaults to standard output)"
     (set! output file)]
    [("-c" "--command-marker") marker "command marker (defaults to \"@\")"
     (command-marker marker)]
    [("--run") cmd "run the command string on a single preprocessed input file"
     (set! run-cmd cmd)])
   (help-labels "   (see the documentation for this option)")
   (multi
    [("-E" "--eval") expr "evaluates <expr> before processing starts"
     (parameterize ([read-case-sensitive #t])
       (add-eval (read (open-input-string expr))))])
   (=> (lambda (_ . files) (run preprocess run-cmd output files))
       '("input-file")
       (more-help 'mztext "a MzScheme-based preprocessing language"))))

)
