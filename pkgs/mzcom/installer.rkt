(module installer mzscheme
  (require mzlib/process
	   setup/dirs)
  (provide post-installer)
  (define (post-installer plt-home)
    (let ([exe "MzCOM.exe"])
      (cond [(not (eq? (system-type) 'windows))
             ;(printf "Warning: can't install MzCOM on non-Windows machine\n")
             (void)]
            [(not (file-exists? (build-path (find-lib-dir) exe)))
             (printf "Warning: MzCOM binary not installed\n")]
            [else (parameterize ([current-directory (find-lib-dir)])
                    (system* exe "/RegServer"))]))))
