
(module pdf-slatex-launcher mzscheme
  (require "slatex-wrapper.ss")

  (define argv (current-command-line-arguments))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (error 'slatex "pdf-slatex not supported under Mac OS Classic")]
    [(windows unix macosx)
     (when (eq? (vector) argv)
       (error 'slatex "expected a file on the command line~n"))
     (parameterize ([error-escape-handler exit])
       (pdf-slatex (vector-ref argv 0)))
     (exit)]))

