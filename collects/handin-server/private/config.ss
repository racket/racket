(module config mzscheme
  (require (lib "file.ss") (lib "list.ss"))

  ;; This module should be invoked when we're in the server directory
  (provide server-dir)
  (define server-dir (current-directory))

  (define config-file (path->complete-path "config.ss" server-dir))

  (define poll-freq 2000.0) ; poll at most once every two seconds

  (define last-poll     #f)
  (define last-filetime #f)
  (define raw-config    #f)
  (define config-cache  #f)

  (provide get-conf)
  (define (get-conf key)
    (unless (and raw-config
                 (< (- (current-inexact-milliseconds) last-poll) poll-freq))
      (set! last-poll (current-inexact-milliseconds))
      (printf "polling...\n")
      (let ([filetime (file-or-directory-modify-seconds config-file)])
        (unless (and filetime (equal? filetime last-filetime))
          (set! last-filetime filetime)
          (set! raw-config
                (with-handlers ([void (lambda (_)
                                        (error 'get-conf
                                               "could not read conf (~a)"
                                               config-file))])
                  (printf "reading...\n")
                  (with-input-from-file config-file read)))
          (set! config-cache (make-hash-table)))))
    (hash-table-get config-cache key
      (lambda ()
        (let*-values ([(default translate) (config-default+translate key)]
                      ;; translate = #f => this is a computed value
                      [(v) (if translate
                             (translate (cond [(assq key raw-config) => cadr]
                                              [else default]))
                             default)])
          (hash-table-put! config-cache key v)
          v))))

  (define (id x) x)
  (define (rx s) (if (regexp? s) s (regexp s)))
  (define (path p) (path->complete-path p server-dir))
  (define (path/false p) (and p (path p)))

  (define (config-default+translate which)
    (case which
      [(port-number)             (values 7979                  id           )]
      [(https-port-number)       (values (add1 (get-conf 'port-number))  id )]
      [(session-timeout)         (values 300                   id           )]
      [(session-memory-limit)    (values 40000000              id           )]
      [(default-file-name)       (values "handin.scm"          id           )]
      [(max-upload)              (values 500000                id           )]
      [(max-upload-keep)         (values 9                     id           )]
      [(user-regexp)             (values #rx"^[a-z][a-z0-9]+$" rx           )]
      [(user-desc)               (values "alphanumeric string" id           )]
      [(username-case-sensitive) (values #f                    id           )]
      [(allow-new-users)         (values #f                    id           )]
      [(allow-change-info)       (values #f                    id           )]
      [(master-password)         (values #f                    id           )]
      [(web-base-dir)            (values #f                    path/false   )]
      [(log-output)              (values #t                    id           )]
      [(log-file)                (values "log"                 path/false   )]
      [(web-log-file)            (values #f                    path/false   )]
      [(extra-fields)
       (values '(("Full Name" #f #f)
                 ("ID#" #f #f)
                 ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
                  "a valid email address"))
               id)]
      ;; computed from the above (mark by translate = #f)
      [(user-fields)
       (values (filter (lambda (f) (not (eq? '- (cadr f))))
                       (get-conf 'extra-fields))
               #f)]
      [else (error 'get-conf "unknown configuration entry: ~s" which)]))

  )
