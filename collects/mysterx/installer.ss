(module installer mzscheme
  (require (lib "process.ss"))
  (provide post-installer)
  (define (post-installer mx-path)
    (define (make-dll-path . more)
      (apply build-path (collection-path "mysterx")
             "private" "compiled" "native" "win32" "i386" more))
    (define (warn fmt . args) (apply fprintf (current-error-port) fmt args))
    (let* ([dlls '("myspage.dll" "myssink.dll")]
           [dll-paths (map make-dll-path dlls)]
           [winsys-dir (find-system-path 'sys-dir)]
           [regsvr (and winsys-dir (build-path winsys-dir "REGSVR32.EXE"))])
      (cond
       [(not (eq? (system-type) 'windows))
        (printf "Warning: can't install MysterX on non-Windows machine\n")]
       [(not (andmap file-exists? dll-paths))
        (printf "Warning: MysterX binaries not installed\n")]
       [(not winsys-dir)
        (printf "Warning: Can't run REGSVR32 on libraries\n")]
       [else (parameterize ([current-directory (make-dll-path)])
               (for-each
                (lambda (dll)
                  (printf "MysterX: ~a library ~a\n"
                          (if (eq? 0 (system*/exit-code regsvr "/s" dll))
                            "Registered" "Unable to register")
                          dll))
                dlls))]))))
