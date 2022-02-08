
;; If checking a boot file fails, output is written to stderr, but the
;; build does not fail. The idea is that stderr output will be good
;; enough to flag a problem in continuous integration, but if the
;; problem happens, it's only an inefficiency and not fatal.

(guard [x [#t (display-condition x (current-error-port))
              (newline (current-error-port))]]
       (#%$fasl-file-equal? (car (command-line-arguments))
                            (cadr (command-line-arguments))
                            #t
                            #t)
       ;; if we get here, there was no error
       (printf "Boot file ~s was correctly created as a fixpoint\n"
               (car (command-line-arguments))))
