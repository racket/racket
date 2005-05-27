(module installer mzscheme
  (require (lib "process.ss"))
  (provide post-installer)
  (define (post-installer plt-home)
    (let ([exe "MzCOM.exe"])
      (cond [(not (eq? (system-type) 'windows))
             (printf "Warning: can't install MzCOM on non-Windows machine\n")]
            [(not (file-exists? (build-path plt-home exe)))
             (printf "Warning: MzCOM binary not installed\n")]
            [else (parameterize ([current-directory plt-home])
                    (system* exe "/RegServer"))]))))
