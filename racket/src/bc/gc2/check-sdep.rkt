(module xform '#%kernel
  (#%require '#%utils
             '#%paramz)

  (define-values (loop)
    (lambda (paths)
      (if (null? paths)
          (void)
          (let-values ([(path) (build-path "xsrc" (car paths))])
            (if (regexp-match? #rx"[.][ch]$" path)
                (let-values ()
                  (define-values (ts) (file-or-directory-modify-seconds path))
                  (define-values (sdep) (path-replace-extension path ".sdep"))
                  (call-with-escape-continuation
                   (lambda (esc)
                     (with-continuation-mark
                         exception-handler-key
                       (lambda (exn)
                         (if (exn:fail? exn)
                             (begin
                               (printf "~a\n removing ~a\n"
                                       (exn-message exn)
                                       path)
                               (delete-file path)
                               (esc))
                             exn))
                       (let-values ()
                         (define-values (dloop)
                           (lambda (paths)
                             (if (null? paths)
                                 (void)
                                 (let-values ()
                                   (define-values (ts2) (file-or-directory-modify-seconds (bytes->path (car paths))))
                                   (if (ts2 . > . ts)
                                       (error 'changed-dependency "~a" (car paths))
                                       (dloop (cdr paths)))))))
                         (dloop (call-with-input-file sdep read))))))
                  (loop (cdr paths)))
                (loop (cdr paths)))))))

  (if (directory-exists? "xsrc")
      (loop (directory-list "xsrc"))
      (void)))
