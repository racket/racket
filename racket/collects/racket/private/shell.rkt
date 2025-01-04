#lang racket/base

(provide shell-path/args)

(define (shell-path/args who argstr)
  (case (system-type)
    [(unix macosx) (append '("/bin/sh" "-c") (list argstr))]
    [(windows) (let ([cmd
                      (let ([d (find-system-path 'sys-dir)])
                        (let ([cmd (build-path d "cmd.exe")])
                          (if (file-exists? cmd)
                            cmd
                            (let ([cmd (build-path d "command.com")])
                              (if (file-exists? cmd)
                                cmd
                                ;; One last try: up a dir
                                (build-path d 'up "command.com"))))))])
                 (list cmd
                       'exact
                       (format "~a /c \"~a\"" (path->string cmd) argstr)))]
    [else (raise-mismatch-error
           who
           (format "~a: don't know what shell to use for platform: " who)
           (system-type))]))

