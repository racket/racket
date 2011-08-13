(module kw-file "pre-base.rkt" 

  (require (prefix-in k: "pre-base.rkt"))

  (provide (rename-out
            [open-input-file        -open-input-file]
            [open-output-file       -open-output-file]
            [open-input-output-file -open-input-output-file]
            [call-with-input-file   -call-with-input-file]
            [call-with-output-file  -call-with-output-file]
            [with-input-from-file   -with-input-from-file]
            [with-output-to-file    -with-output-to-file])
           call-with-input-file*
           call-with-output-file*)

  (define exists-syms
    '(error append update can-update replace truncate must-truncate truncate/replace))

  (define exists-desc
    "'error, 'append, 'update, 'can-update, 'replace, 'truncate, 'must-truncate, or 'truncate/replace")

  (define (open-input-file path #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-type-error 'open-input-file "path or string" path))
    (unless (memq mode '(binary text))
      (raise-type-error 'open-input-file "'binary or 'text" mode))
    (k:open-input-file path mode))

  (define (open-output-file path #:mode [mode 'binary]
                            #:exists [exists 'error])
    (unless (path-string? path)
      (raise-type-error 'open-output-file "path or string" path))
    (unless (memq mode '(binary text))
      (raise-type-error 'open-output-file "'binary or 'text" mode))
    (unless (memq exists exists-syms)
      (raise-type-error 'open-output-file exists-desc exists))
    (k:open-output-file path mode exists))

  (define (open-input-output-file path #:mode [mode 'binary]
                                  #:exists [exists 'error])
    (unless (path-string? path)
      (raise-type-error 'open-input-output-file "path or string" path))
    (unless (memq mode '(binary text))
      (raise-type-error 'open-input-output-file "'binary or 'text" mode))
    (unless (memq exists exists-syms)
      (raise-type-error 'open-input-output-file exists-desc exists))
    (k:open-input-output-file path mode exists))

  (define (call-with-input-file path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-type-error 'call-with-input-file "path or string" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-type-error 'call-with-input-file "procedure (arity 1)" proc))
    (unless (memq mode '(binary text))
      (raise-type-error 'call-with-input-file "'binary or 'text" mode))
    (k:call-with-input-file path proc mode))

  (define (call-with-output-file path proc
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
    (k:call-with-output-file path proc mode exists))

  (define (with-input-from-file path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-type-error 'with-input-from-file "path or string" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 0))
      (raise-type-error 'with-input-from-file "procedure (arity 0)" proc))
    (unless (memq mode '(binary text))
      (raise-type-error 'with-input-from-file "'binary or 'text" mode))
    (k:with-input-from-file path proc mode))

  (define (with-output-to-file path proc
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
    (k:with-output-to-file path proc mode exists))

  (define (call-with-input-file* path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-type-error 'call-with-input-file* "path or string" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-type-error 'call-with-input-file* "procedure (arity 1)" proc))
    (unless (memq mode '(binary text))
      (raise-type-error 'call-with-input-file* "'binary or 'text" mode))
    (let ([p (k:open-input-file path mode)])
      (dynamic-wind
        void
        (lambda () (proc p))
        (lambda () (close-input-port p)))))

  (define (call-with-output-file* path proc 
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
      (let ([p (k:open-output-file path mode exists)])
        (dynamic-wind
            void
            (lambda () (proc p))
            (lambda () (close-output-port p))))))
