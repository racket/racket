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
           call-with-output-file*
           (rename-out
            [directory-list -directory-list]))

  (define exists-syms
    '(error append update can-update replace truncate must-truncate truncate/replace))

  (define exists-desc
    "(or/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)")
  (define binary-or-text-desc
    "(or/c 'binary 'text)")

  (define (open-input-file path #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-argument-error 'open-input-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-input-file binary-or-text-desc mode))
    (k:open-input-file path mode))

  (define (open-output-file path #:mode [mode 'binary]
                            #:exists [exists 'error])
    (unless (path-string? path)
      (raise-argument-error 'open-output-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'open-output-file exists-desc exists))
    (k:open-output-file path mode exists))

  (define (open-input-output-file path #:mode [mode 'binary]
                                  #:exists [exists 'error])
    (unless (path-string? path)
      (raise-argument-error 'open-input-output-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-input-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'open-input-output-file exists-desc exists))
    (k:open-input-output-file path mode exists))

  (define (call-with-input-file path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-argument-error 'call-with-input-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-argument-error 'call-with-input-file "(input-port? . -> . any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'call-with-input-file binary-or-text-desc mode))
    (k:call-with-input-file path proc mode))

  (define (call-with-output-file path proc
                                 #:mode [mode 'binary]
                                 #:exists [exists 'error])
    (unless (path-string? path)
      (raise-argument-error 'call-with-output-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-argument-error 'call-with-output-file "(output-port? . -> . any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'call-with-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'call-with-output-file exists-desc exists))
    (k:call-with-output-file path proc mode exists))

  (define (with-input-from-file path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-argument-error 'with-input-from-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 0))
      (raise-argument-error 'with-input-from-file "(-> any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'with-input-from-file binary-or-text-desc mode))
    (k:with-input-from-file path proc mode))

  (define (with-output-to-file path proc
                               #:mode [mode 'binary]
                               #:exists [exists 'error])
    (unless (path-string? path)
      (raise-argument-error 'with-output-to-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 0))
      (raise-argument-error 'with-output-to-file "(-> any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'with-output-to-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'with-output-to-file exists-desc exists))
    (k:with-output-to-file path proc mode exists))

  (define (call-with-input-file* path proc #:mode [mode 'binary])
    (unless (path-string? path)
      (raise-argument-error 'call-with-input-file* "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-argument-error 'call-with-input-file* "(input-port? . -> . any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'call-with-input-file* binary-or-text-desc mode))
    (let ([p (k:open-input-file path mode)])
      (dynamic-wind
        void
        (lambda () (proc p))
        (lambda () (close-input-port p)))))

  (define (call-with-output-file* path proc 
                                  #:mode [mode 'binary]
                                  #:exists [exists 'error])
      (unless (path-string? path)
        (raise-argument-error 'call-with-output-file* "path-string?" path))
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-argument-error 'call-with-output-file* "(output-port? . -> . any)" proc))
      (unless (memq mode '(binary text))
        (raise-argument-error 'call-with-output-file* binary-or-text-desc mode))
      (unless (memq exists exists-syms)
        (raise-argument-error 'call-with-output-file* exists-desc exists))
      (let ([p (k:open-output-file path mode exists)])
        (dynamic-wind
            void
            (lambda () (proc p))
            (lambda () (close-output-port p)))))

  ;; Using `define-values' to avoid the inlining expansion for keyword
  ;; arguments, because that expansion confuses Typed Racket:
  (define-values (directory-list)
    (lambda ([dir (current-directory)] #:build? [build? #f])
      (unless (path-string? dir)
        (raise-argument-error 'directory-list "path-string?" dir))
      (if build?
          (map (lambda (i) (build-path dir i)) (k:directory-list dir))
          (k:directory-list dir)))))
