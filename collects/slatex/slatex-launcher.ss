
(module slatex-launcher mzscheme
  (require "slatex-wrapper.ss")

  (define argv (current-command-line-arguments))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (current-load slatex)
     
     (for-each slatex (vector->list argv))]
    [(windows unix macosx)
     (when (eq? (vector) argv)
       (fprintf (current-error-port) "slatex: expected a file on the command line\n")
       (exit 1))
     (let ([result
            (parameterize ([error-escape-handler exit])
              (slatex (vector-ref argv 0)))])
       (if result
           (exit)
           (exit 1)))]))
