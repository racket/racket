(module slatex-launcher scheme/base
  (require "slatex-wrapper.rkt"
           scheme/cmdline)

  (define argv (current-command-line-arguments))
  
  (define no-latex (make-parameter #f))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (current-load slatex)
     
     (for-each slatex (vector->list argv))]
    [(windows unix macosx)
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
                    (slatex filename)))])
       (if result
           (exit)
           (exit 1)))]))
