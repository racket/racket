(module pp-run mzscheme

(require (lib "process.ss"))

(provide more-help)
(define (more-help name top-line)
  (lambda (help)
    (printf "This is `~a', ~a.\nUsage: " name top-line)
    (display (regexp-replace
              #rx"\n where" help
              "\n where an \"-\" input file specifies standard input\n and"))
    (display "See \"plt/preprocessor/doc.txt\" for more details.\n")
    (exit 0)))

(provide run)
(define (run preprocess run-cmd output files)
  (let ([files (map (lambda (f) (if (equal? f "-") (current-input-port) f))
                    (if (null? files) '("-") files))]
        [exit-code 0])
    (define (do-run-subst f)
      (set! exit-code (system/exit-code (regexp-replace
                                         #rx"\\*" run-cmd (format "~s" f)))))
    (cond
     [(and run-cmd (not (regexp-match #rx"\\*" run-cmd)))
      (when output
        (error 'mzpp "cannot run a command with piped stdin when an ~a"
               "output name is specified"))
      (let ([p (process/ports (current-output-port) #f (current-error-port)
                              run-cmd)])
        (parameterize ([current-output-port (list-ref p 1)])
          (apply preprocess files))
        (close-output-port (list-ref p 1))
        ((list-ref p 4) 'wait)
        (set! exit-code ((list-ref p 4) 'exit-code)))]
     [(and run-cmd (not (or (= 1 (length files)) output)))
      (error 'mzpp "cannot run a command that expects a filename with ~a"
             "multiple input files and no output name")]
     [(and run-cmd (not output))
      (let* ([file (car files)]
             [temp (format "~a-mzpp-temporary" file)])
        (when (file-exists? temp)
          (error 'mzpp "~s already exists!" temp))
        (dynamic-wind
          (lambda () (rename-file-or-directory file temp))
          (lambda ()
            (with-output-to-file file (lambda () (preprocess temp)))
            (do-run-subst file))
          (lambda ()
            (delete-file file)
            (rename-file-or-directory temp file))))]
     [output
      (with-output-to-file output (lambda () (apply preprocess files)) 'replace)
      (when run-cmd (do-run-subst output))]
     [else (apply preprocess files)])
    (exit exit-code)))

)
