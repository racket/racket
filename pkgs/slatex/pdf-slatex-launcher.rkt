(module pdf-slatex-launcher mzscheme
  (require "slatex-wrapper.rkt"
           scheme/cmdline)

  (module test racket/base)

  (define argv (current-command-line-arguments))
  
  (define no-latex (make-parameter #f))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (error 'slatex "pdf-slatex not supported under Mac OS Classic")]
    [(windows unix macosx)
     (when (equal? (vector) argv)
       (eprintf "pdf-slatex: expected a file on the command line\n")
       (exit 1))
     (let* ([filename
             (command-line
              #:program "slatex"
              #:once-each
              [("-n" "--no-latex") "Just preprocess, don't run LaTeX"
                                   (no-latex #t)]
              #:args (filename)
              filename)]
            [result
             (parameterize ([error-escape-handler exit])
               (if (no-latex)
                   (slatex/no-latex filename)
                   (pdf-slatex filename)))])
       (if result
           (exit)
           (exit 1)))]))

