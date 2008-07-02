
(module pdf-slatex-launcher mzscheme
  (require "slatex-wrapper.ss")

  (define argv (current-command-line-arguments))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (error 'slatex "pdf-slatex not supported under Mac OS Classic")]
    [(windows unix macosx)
     (when (equal? (vector) argv)
       (fprintf (current-error-port) "pdf-slatex: expected a file on the command line\n")
       (exit 1))
     (let-values ([(nonstop? file) (if (string=? "\\nonstopmode" (vector-ref argv 0))
                                       (values #t (vector-ref argv 1))
                                       (values #f (vector-ref argv 0)))])
     (let ([result
            (parameterize ([error-escape-handler exit]
                           [nonstop-mode? nonstop?])
              (pdf-slatex file))])
       (if result
           (exit)
           (exit 1))))]))

