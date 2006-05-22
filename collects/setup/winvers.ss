;; This is a wrapper around `winvers-change.ss' to patch binary files with the
;; current version number.
(module winvers mzscheme
  (require (lib "file.ss") 
	   "main-collects.ss"
	   "dirs.ss")

  (define (make-copy)
    (let* ([tmpdir (find-system-path 'temp-dir)]
           [vers (build-path tmpdir "setvers")])
      (unless (directory-exists? vers) (make-directory vers))
      (for-each (lambda (p)
                  (let ([dest (build-path vers p)])
                    ((cond [(file-exists? dest) delete-file]
                           [(directory-exists? dest) delete-directory/files]
                           [else void])
                     dest)
                    (copy-directory/files (build-path (find-console-bin-dir) p)
                                          dest)))
                '("mzscheme.exe" "lib"))
      (build-path vers "mzscheme.exe")))

  (define (patch-files)
    (parameterize ((current-command-line-arguments
                    (vector (path->string (find-console-bin-dir)))))
      (dynamic-require `(lib "winvers-change.ss" "setup") #f)))

  (define collects-dir
    (path->string (find-collects-dir)))

  (let ([argv (current-command-line-arguments)])
    (cond
      [(equal? argv #())
       (let ([exe (make-copy)])
         (printf "re-launching first time...~n")
         (subprocess
          (current-output-port) (current-input-port) (current-error-port)
          exe "--collects" collects-dir
          "-mvqL-" "winvers.ss" "setup" "patch"))]
      [(equal? argv #("patch"))
       (sleep 1) ; time for other process to end
       (patch-files)
       (printf "re-launching last time...~n")
       (subprocess
        (current-output-port) (current-input-port) (current-error-port)
        (build-path (find-console-bin-dir) "mzscheme.exe")
        "-mvqL-" "winvers.ss" "setup" "finish")]
      [(equal? argv #("finish"))
       (sleep 1) ; time for other process to end
       (delete-directory/files
        (build-path (find-system-path 'temp-dir) "setvers"))
       (printf "done!~n")]
      [else
       (error 'winvers "unknown command line: ~e" argv)])))
