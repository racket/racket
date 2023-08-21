(module kw-file "pre-base.rkt" 

  (require (prefix-in k: "pre-base.rkt")
           "sort.rkt")

  (provide (rename-out
            [open-input-file        -open-input-file]
            [open-output-file       -open-output-file]
            [open-input-output-file -open-input-output-file]
            [call-with-input-file   -call-with-input-file]
            [call-with-output-file  -call-with-output-file]
            [with-input-from-file   -with-input-from-file]
            [with-output-to-file    -with-output-to-file]
            [raise-syntax-error     -raise-syntax-error])
           call-with-input-file*
           call-with-output-file*
           (rename-out
            [directory-list -directory-list]
            [copy-file -copy-file]))

  (define exists-syms
    '(error append update can-update replace truncate must-truncate truncate/replace))

  (define exists-desc
    "(or/c 'error 'append 'update 'can-update 'replace 'truncate 'must-truncate 'truncate/replace)")
  (define binary-or-text-desc
    "(or/c 'binary 'text)")

  (define DEFAULT-CREATE-PERMS #o666)
  (define (permissions? perms)
    (and (exact-integer? perms) (<= 0 perms 65535)))
  (define perms-desc "(integer-in 0 65535)")

  (define (open-input-file path #:mode [mode 'binary] #:for-module? [for-module? #f])
    (unless (path-string? path)
      (raise-argument-error 'open-input-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-input-file binary-or-text-desc mode))
    (k:open-input-file path mode (if for-module? 'module 'none)))

  (define (open-output-file path #:mode [mode 'binary]
                            #:exists [exists 'error]
                            #:permissions [perms DEFAULT-CREATE-PERMS]
                            #:replace-permissions? [replace-permissions? #f])
    (unless (path-string? path)
      (raise-argument-error 'open-output-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'open-output-file exists-desc exists))
    (unless (permissions? perms)
      (raise-argument-error 'open-output-file perms-desc perms))
    (k:open-output-file path mode exists perms  (and replace-permissions? 'replace-permissions)))

  (define (open-input-output-file path #:mode [mode 'binary]
                                  #:exists [exists 'error]
                                  #:permissions [perms DEFAULT-CREATE-PERMS]
                                  #:replace-permissions? [replace-permissions? #f])
    (unless (path-string? path)
      (raise-argument-error 'open-input-output-file "path-string?" path))
    (unless (memq mode '(binary text))
      (raise-argument-error 'open-input-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'open-input-output-file exists-desc exists))
    (unless (permissions? perms)
      (raise-argument-error 'open-input-output-file perms-desc perms))
    (k:open-input-output-file path mode exists perms (and replace-permissions? 'replace-permissions)))

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
                                 #:exists [exists 'error]
                                 #:permissions [perms DEFAULT-CREATE-PERMS]
                                 #:replace-permissions? [replace-permissions? #f])
    (unless (path-string? path)
      (raise-argument-error 'call-with-output-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 1))
      (raise-argument-error 'call-with-output-file "(output-port? . -> . any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'call-with-output-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'call-with-output-file exists-desc exists))
    (unless (permissions? perms)
      (raise-argument-error 'call-with-output-file perms-desc perms))
    (k:call-with-output-file path proc mode exists perms (and replace-permissions? 'replace-permissions)))

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
                               #:exists [exists 'error]
                               #:permissions [perms DEFAULT-CREATE-PERMS]
                               #:replace-permissions? [replace-permissions? #f])
    (unless (path-string? path)
      (raise-argument-error 'with-output-to-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 0))
      (raise-argument-error 'with-output-to-file "(-> any)" proc))
    (unless (memq mode '(binary text))
      (raise-argument-error 'with-output-to-file binary-or-text-desc mode))
    (unless (memq exists exists-syms)
      (raise-argument-error 'with-output-to-file exists-desc exists))
    (unless (permissions? perms)
      (raise-argument-error 'with-output-to-file perms-desc perms))
    (k:with-output-to-file path proc mode exists perms (and replace-permissions? 'replace-permissions)))

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
                                  #:exists [exists 'error]
                                  #:permissions [perms DEFAULT-CREATE-PERMS]
                                  #:replace-permissions? [replace-permissions? #f])
      (unless (path-string? path)
        (raise-argument-error 'call-with-output-file* "path-string?" path))
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-argument-error 'call-with-output-file* "(output-port? . -> . any)" proc))
      (unless (memq mode '(binary text))
        (raise-argument-error 'call-with-output-file* binary-or-text-desc mode))
      (unless (memq exists exists-syms)
        (raise-argument-error 'call-with-output-file* exists-desc exists))
      (unless (permissions? perms)
        (raise-argument-error 'call-with-output-file* perms-desc perms))
      (let ([p (k:open-output-file path mode exists perms (and replace-permissions? 'replace-permissions))])
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
      (let ([content (sort (k:directory-list dir)
                           path<?)])
        (if build?
            (map (lambda (i) (build-path dir i)) content)
            content))))

  (define-values (copy-file)
    (let ([not-supplied exists-syms])
      (lambda (src dest [exists-ok? not-supplied]
                   #:exists-ok? [exists-ok?/kw not-supplied]
                   #:permissions [perms #f]
                   #:replace-permissions? [replace-permissions? #t])
        (unless (or (eq? exists-ok? not-supplied)
                    (eq? exists-ok?/kw not-supplied))
          (raise-arguments-error 'copy-file "cannot supply both non-keyword and keyword `exists-ok?` argument"
                                 "by-position argument" exists-ok?
                                 "keyword argument" exists-ok?/kw))
        (k:copy-file src dest
                     (if (eq? exists-ok? not-supplied)
                         (if (eq? exists-ok?/kw not-supplied)
                             #f
                             exists-ok?/kw)
                         exists-ok?)
                     perms replace-permissions?))))

  (define (raise-syntax-error given-name message
                              [expr #f] [sub-expr #f]
                              [extra-sources null]
                              [message-suffix ""]
                              #:exn [exn exn:fail:syntax])
    (do-raise-syntax-error 'raise-syntax-error exn given-name message
                           expr sub-expr
                           extra-sources
                           message-suffix)))
