(module config mzscheme
  (require (lib "file.ss"))

  ;; This module should be invoked when in the server directory
  (provide server-dir)
  (define server-dir (current-directory))

  (define config-file (path->complete-path "config.ss" server-dir))

  (define (get-config* which default)
    (if (file-exists? config-file)
      (get-preference which (lambda () default) #f config-file)
      default))

  (define (id x) x)
  (define (rx s) (if (regexp? s) s (regexp s)))
  (define (path p) (path->complete-path p server-dir))
  (define (path/false p) (and p (path p)))

  (define (config-default+translate which)
    (case which
      [(port-number)              (values 7979                  id           )]
      [(https-port-number)        (values (add1 (get-config 'port-number)) id)]
      [(session-timeout)          (values 300                   id           )]
      [(session-memory-limit)     (values 40000000              id           )]
      [(default-file-name)        (values "handin.scm"          id           )]
      [(max-upload)               (values 500000                id           )]
      [(max-upload-keep)          (values 9                     id           )]
      [(user-regexp)              (values #rx"^[a-z][a-z0-9]+$" rx           )]
      [(user-desc)                (values "alphanumeric string" id           )]
      [(username-case-sensitive?) (values #f                    id           )]
      [(allow-new-users)          (values #f                    id           )]
      [(allow-change-info)        (values #f                    id           )]
      [(master-password)          (values #f                    id           )]
      [(web-base-dir)             (values #f                    path/false   )]
      [(log-output)               (values #t                    id           )]
      [(log-file)                 (values "log"                 path/false   )]
      [(web-log-file)             (values #f                    path/false   )]
      [(extra-fields)
       (values '(("Full Name" #f #f)
                 ("ID#" #f #f)
                 ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
                  "a valid email address"))
               id)]
      [else (error 'get-config "unknown configuration entry: ~s" which)]))

  (provide get-config)
  (define (get-config which)
    (let-values ([(default translate) (config-default+translate which)])
      (translate (get-config* which default)))))
