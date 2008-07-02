
(module slatex-launcher mzscheme
  (require "slatex-wrapper.ss")

  (define argv (current-command-line-arguments))
  
  (case (system-type)
    [(macos)
     
     ;; set up drag and drop
     (current-load slatex)
     
     (for-each slatex (vector->list argv))]
    [(windows unix macosx)
     (when (equal? (vector) argv)
       (fprintf (current-error-port) "slatex: expected a file on the command line\n")
       (exit 1))
     (let-values ([(nonstop? file) (if (string=? "\\nonstopmode" (vector-ref argv 0))
                                       (values #t (vector-ref argv 1))
                                       (values #f (vector-ref argv 0)))])
     (let ([result
            (parameterize ([error-escape-handler exit]
                           [nonstop-mode? nonstop?])
              (slatex file))])
       (if result
           (exit)
           (exit 1))))]))
