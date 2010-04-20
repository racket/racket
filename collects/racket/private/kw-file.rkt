
(module kw-file "pre-base.rkt" 

  (provide -open-input-file
           -open-output-file
           -open-input-output-file
           -call-with-input-file
           -call-with-output-file
           -with-input-from-file
           -with-output-to-file
           call-with-input-file*
           call-with-output-file*)

  (define exists-syms
    '(error append update can-update replace truncate must-truncate truncate/replace))

  (define exists-desc
    "'error, 'append, 'update, 'can-update, 'replace, 'truncate, 'must-truncate, or 'truncate/replace")

  (define -open-input-file
    (let ([open-input-file (lambda (path #:mode [mode 'binary])
                             (unless (path-string? path)
                               (raise-type-error 'open-input-file "path or string" path))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'open-input-file "'binary or 'text" mode))
                             (open-input-file path mode))])
      open-input-file))

  (define -open-output-file
    (let ([open-output-file (lambda (path #:mode [mode 'binary]
                                          #:exists [exists 'error])
                             (unless (path-string? path)
                               (raise-type-error 'open-output-file "path or string" path))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'open-output-file "'binary or 'text" mode))
                             (unless (memq exists exists-syms)
                               (raise-type-error 'open-output-file exists-desc exists))
                             (open-output-file path mode exists))])
      open-output-file))

  (define -open-input-output-file
    (let ([open-input-output-file (lambda (path #:mode [mode 'binary]
                                                #:exists [exists 'error])
                                    (unless (path-string? path)
                                      (raise-type-error 'open-input-output-file "path or string" path))
                                    (unless (memq mode '(binary text))
                                      (raise-type-error 'open-input-output-file "'binary or 'text" mode))
                                    (unless (memq exists exists-syms)
                                      (raise-type-error 'open-input-output-file exists-desc exists))
                                    (open-input-output-file path mode exists))])
      open-input-output-file))

  (define -call-with-input-file
    (let ([call-with-input-file (lambda (path proc #:mode [mode 'binary])
                             (unless (path-string? path)
                               (raise-type-error 'call-with-input-file "path or string" path))
                             (unless (and (procedure? proc)
                                          (procedure-arity-includes? proc 1))
                               (raise-type-error 'call-with-input-file "procedure (arity 1)" proc))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'call-with-input-file "'binary or 'text" mode))
                             (call-with-input-file path proc mode))])
      call-with-input-file))

  (define -call-with-output-file
    (let ([call-with-output-file (lambda (path proc 
                                               #:mode [mode 'binary]
                                               #:exists [exists 'error])
                             (unless (path-string? path)
                               (raise-type-error 'call-with-output-file "path or string" path))
                             (unless (and (procedure? proc)
                                          (procedure-arity-includes? proc 1))
                               (raise-type-error 'call-with-output-file "procedure (arity 1)" proc))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'call-with-output-file "'binary or 'text" mode))
                             (unless (memq exists exists-syms)
                               (raise-type-error 'call-with-output-file exists-desc exists))
                             (call-with-output-file path proc mode exists))])
      call-with-output-file))
  
  (define -with-input-from-file
    (let ([with-input-from-file (lambda (path proc #:mode [mode 'binary])
                             (unless (path-string? path)
                               (raise-type-error 'with-input-from-file "path or string" path))
                             (unless (and (procedure? proc)
                                          (procedure-arity-includes? proc 0))
                               (raise-type-error 'with-input-from-file "procedure (arity 0)" proc))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'with-input-from-file "'binary or 'text" mode))
                             (with-input-from-file path proc mode))])
      with-input-from-file))

  (define -with-output-to-file
    (let ([with-output-to-file (lambda (path proc
                                             #:mode [mode 'binary]
                                             #:exists [exists 'error])
                             (unless (path-string? path)
                               (raise-type-error 'with-output-to-file "path or string" path))
                             (unless (and (procedure? proc)
                                          (procedure-arity-includes? proc 0))
                               (raise-type-error 'with-output-to-file "procedure (arity 0)" proc))
                             (unless (memq mode '(binary text))
                               (raise-type-error 'with-output-to-file "'binary or 'text" mode))
                             (unless (memq exists exists-syms)
                               (raise-type-error 'with-output-to-file exists-desc exists))
                             (with-output-to-file path proc mode exists))])
      with-output-to-file))

  (define call-with-input-file*
    (lambda (path proc #:mode [mode 'binary])
      (unless (path-string? path)
        (raise-type-error 'call-with-input-file* "path or string" path))
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-type-error 'call-with-input-file* "procedure (arity 1)" proc))
      (unless (memq mode '(binary text))
        (raise-type-error 'call-with-input-file* "'binary or 'text" mode))
      (let ([p (open-input-file path mode)])
        (dynamic-wind
            void
            (lambda () (proc p))
            (lambda () (close-input-port p))))))

  (define call-with-output-file*
    (lambda (path proc 
                  #:mode [mode 'binary]
                  #:exists [exists 'error])
      (unless (path-string? path)
        (raise-type-error 'call-with-output-file* "path or string" path))
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-type-error 'call-with-output-file* "procedure (arity 1)" proc))
      (unless (memq mode '(binary text))
        (raise-type-error 'call-with-output-file* "'binary or 'text" mode))
      (unless (memq exists exists-syms)
        (raise-type-error 'call-with-output-file* exists-desc exists))
      (let ([p (open-output-file path mode exists)])
        (dynamic-wind
            void
            (lambda () (proc p))
            (lambda () (close-output-port p)))))))
