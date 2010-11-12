(module build racket/base
  (require racket/path 
           racket/file
           dynext/file
           dynext/link
           dynext/compile)

  (define-values (libname c-files)
    (let ([l (vector->list (current-command-line-arguments))])
      (values (car l)
              (cdr l))))

  (define sys-subpath (system-library-subpath #f))

  (define so-name (append-extension-suffix libname))
  (parameterize (;; we compile a simple .so, not an extension
                 [current-standard-link-libraries '()])
    (when (or (not (file-exists? so-name))
              (let ([so-time (file-or-directory-modify-seconds so-name)])
                (for/or ([f c-files])
                  ((file-or-directory-modify-seconds f) . > . so-time))))
      (let ([o-files
             (for/list ([c-file c-files])
               (let ([o-file (append-object-suffix (path-replace-suffix (file-name-from-path c-file) #""))])
                 ;; first #f means not quiet (here and in link-extension)
                 (compile-extension #f c-file o-file null)
                 o-file))])
        (let* ([flags (if (string=? "i386-cygwin" (path->string sys-subpath))
                          ;; DLL needs every dependence explicit:
                          '("-lc" "-lm" "-lcygwin" "-lkernel32")
                          null)])
          (link-extension #f (append o-files flags) so-name))))))
