#lang racket/base
(require "../common/check.rkt"
         "../common/resource.rkt"
         "../path/path.rkt"
         "../path/parameter.rkt"
         "../path/directory-path.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/error.rkt"
         "../format/main.rkt"
         "../security/main.rkt"
         "parameter.rkt"
         "host.rkt"
         "identity.rkt"
         "error.rkt"
         (only-in "error.rkt"
                  set-maybe-raise-missing-module!))

(provide directory-exists?
         file-exists?
         link-exists?
         make-directory
         directory-list
         current-force-delete-permissions
         delete-file
         delete-directory
         rename-file-or-directory
         file-or-directory-modify-seconds
         file-or-directory-permissions
         file-or-directory-identity
         file-size
         copy-file
         make-file-or-directory-link
         resolve-path
         expand-user-path
         filesystem-root-list

         ;; For the expander to register `maybe-raise-missing-module`:
         set-maybe-raise-missing-module!
	 ;; To resolve a cycle with the definition of `simplify-path`
	 set-simplify-path-for-directory-list!)

(define/who (directory-exists? p)
  (check who path-string? p)
  (rktio_directory_exists rktio (->host p who '(exists))))

(define/who (file-exists? p)
  (check who path-string? p)
  (rktio_file_exists rktio (->host p who '(exists))))

(define/who (link-exists? p)
  (check who path-string? p)
  (rktio_link_exists rktio (->host p who '(exists))))

(define/who (make-directory p)
  (check who path-string? p)
  (define host-path (->host p who '(write)))
  (define r (rktio_make_directory rktio host-path))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "cannot make directory~a\n"
                                     "  path: ~a")
                                    (if (racket-error? r RKTIO_ERROR_EXISTS)
                                        ";\n the path already exists"
                                        "")
                                    (host-> host-path)))))

(define simplify-path/dl (lambda (p) p))
(define (set-simplify-path-for-directory-list! proc)
  (set! simplify-path/dl proc))

(define/who (directory-list [p (current-directory)])
  (check who path-string? p)
  (define host-path/initial (->host p who '(read)))
  (define host-path (case (system-type)
		      [(windows)
		       ;; Need to avoid "." and "..", so simplify
		       (->host (simplify-path/dl (host-> host-path/initial)) #f '())]
		      [else host-path/initial]))
  (atomically
   (call-with-resource
    (rktio_directory_list_start rktio host-path)
    ;; in atomic mode
    (lambda (dl) (rktio_directory_list_stop rktio dl))
    ;; in atomic mode
    (lambda (dl)
      (cond
        [(rktio-error? dl)
         (end-atomic)
         (raise-filesystem-error who
                                 dl
                                 (format (string-append
                                          "could not open directory\n"
                                          "  path: ~a")
                                         (host-> host-path)))]
        [else
         (end-atomic)
         (let loop ([accum null])
           (start-atomic)
           (define fnp (rktio_directory_list_step rktio dl))
           (define fn (if (rktio-error? fnp)
                          fnp
                          (rktio_to_bytes fnp)))
           (cond
             [(rktio-error? fn)
              (end-atomic)
              (check-rktio-error fn "error reading directory")]
             [(equal? fn #"")
              ;; `dl` is no longer valid; need to return still in
              ;; atomic mode, so that `dl` is not destroyed again
              accum]
             [else
              (rktio_free fnp)
              (end-atomic)
              (loop (cons (host-> fn) accum))]))])))))

(define/who (delete-file p)
  (check who path-string? p)
  (define host-path (->host p who '(delete)))
  (define r (rktio_delete_file rktio
                               host-path
                               (current-force-delete-permissions)))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "cannot delete file\n"
                                     "  path: ~a")
                                    (host-> host-path)))))

(define/who (delete-directory p)
  (check who path-string? p)
  (define host-path (->host p who '(delete)))
  (define r (rktio_delete_directory rktio
                                    host-path
                                    (->host (current-directory) #f #f)
                                    (current-force-delete-permissions)))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "cannot delete directory\n"
                                     "  path: ~a")
                                    (host-> host-path)))))

(define/who (rename-file-or-directory old new [exists-ok? #f])
  (check who path-string? old)
  (check who path-string? new)
  (define host-old (->host old who '(read)))
  (define host-new (->host new who '(write)))
  (define r (rktio_rename_file rktio host-new host-old exists-ok?))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "cannot rename file or directory~a\n"
                                     "  source path: ~a\n"
                                     "  dest path: ~a")
                                    (if (racket-error? r RKTIO_ERROR_EXISTS)
                                        ";\n the destination path already exists"
                                        "")
                                    (host-> host-old)
                                    (host-> host-new)))))

(define/who file-or-directory-modify-seconds
  (case-lambda
    [(p)
     (check who path-string? p)
     (do-file-or-directory-modify-seconds who p #f #f)]
    [(p secs)
     (check who path-string? p)
     (check who exact-integer? secs)
     (do-file-or-directory-modify-seconds who p secs #f)]
    [(p secs fail)
     (check who path-string? p)
     (check who #:or-false exact-integer? secs)
     (check who (procedure-arity-includes/c 0) fail)
     (do-file-or-directory-modify-seconds who p secs fail)]))

(define (do-file-or-directory-modify-seconds who p secs fail)
  (when secs
    (unless (rktio_is_timestamp secs)
      (raise-arguments-error who
                             "integer value is out-of-range"
                             "value" secs)))
  (define host-path (->host p who (if secs '(write) '(read))))
  (start-atomic)
  (define r0 (if secs
                 (rktio_set_file_modify_seconds rktio host-path secs)
                 (rktio_get_file_modify_seconds rktio host-path)))
  (define r (if (and (not secs) (not (rktio-error? r0)))
                (begin0
                  (rktio_timestamp_ref r0)
                  (rktio_free r0))
                r0))
  (end-atomic)
  (cond
    [(rktio-error? r)
     (if fail
         (fail)
         (raise-filesystem-error who
                                 r
                                 (format (string-append
                                          "error ~a file/directory time\n"
                                          "  path: ~a")
                                         (if secs "setting" "getting")
                                         (host-> host-path))))]
    [else r]))

(define/who (file-or-directory-permissions p [mode #f])
  (check who path-string? p)
  (check who (lambda (m)
               (or (not m)
                   (eq? m 'bits)
                   (and (exact-integer? m)
                        (<= 0 m 65535))))
         #:contract "(or/c #f 'bits (integer-in 0 65535))"
         mode)
  (define host-path (->host p who (if (integer? mode) '(write) '(read))))
  (define r
    (if (integer? mode)
        (rktio_set_file_or_directory_permissions rktio host-path mode)
        (rktio_get_file_or_directory_permissions rktio host-path (eq? mode 'bits))))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "~a failed~a\n"
                                     "  path: ~a~a")
                                    (if (integer? mode) "update" "access")
                                    (if (racket-error? r RKTIO_ERROR_EXISTS)
                                        ";\n unsupported bit combination"
                                        "")
                                    (host-> host-path)
                                    (if (racket-error? r RKTIO_ERROR_EXISTS)
                                        (format "\n  permission value: ~a" mode)
                                        ""))))
  (cond
    [(integer? mode) (void)]
    [(eq? 'bits mode) r]
    [else
     (define (set? n) (eqv? n (bitwise-and r n)))
     (let* ([l '()]
            [l (if (set? RKTIO_PERMISSION_READ)
                   (cons 'read l)
                   l)]
            [l (if (set? RKTIO_PERMISSION_WRITE)
                   (cons 'write l)
                   l)]
            [l (if (set? RKTIO_PERMISSION_EXEC)
                   (cons 'execute l)
                   l)])
       l)]))

(define/who (file-or-directory-identity p [as-link? #f])
  (check who path-string? p)
  (define host-path (->host p who '(exists)))
  (start-atomic)
  (path-or-fd-identity who #:host-path host-path #:as-link? as-link?))

(define/who (file-size p)
  (check who path-string? p)
  (define host-path (->host p who '(read)))
  (start-atomic)
  (define r0 (rktio_file_size rktio host-path))
  (define r (if (rktio-error? r0)
                r0
                (begin0
                  (rktio_filesize_ref r0)
                  (rktio_free r0))))
  (end-atomic)
  (cond
    [(rktio-error? r)
     (raise-filesystem-error who
                             r
                             (format (string-append
                                      "cannot get size\n"
                                      "  path: ~a")
                                     (host-> host-path)))]
    [else r]))

(define/who (copy-file src dest [exists-ok? #f])
  (check who path-string? src)
  (check who path-string? dest)
  (define src-host (->host src who '(read)))
  (define dest-host (->host dest who '(write delete)))
  (define (report-error r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "~a\n"
                                     "  source path: ~a\n"
                                     "  destination path: ~a")
                                    (copy-file-step-string r)
                                    (host-> src-host)
                                    (host-> dest-host))))
  (start-atomic)
  (let ([cp (rktio_copy_file_start rktio dest-host src-host exists-ok?)])
    (cond
      [(rktio-error? cp)
       (end-atomic)
       (report-error cp)]
      [else
       (thread-push-kill-callback!
        (lambda () (rktio_copy_file_stop rktio cp)))
       (dynamic-wind
        void
        (lambda ()
          (end-atomic)
          (let loop ()
            (cond
              [(rktio_copy_file_is_done rktio cp)
               (define r (rktio_copy_file_finish_permissions rktio cp))
               (when (rktio-error? r) (report-error r))]
              [else
               (define r (rktio_copy_file_step rktio cp))
               (when (rktio-error? r) (report-error r))
               (loop)])))
        (lambda ()
          (start-atomic)
          (rktio_copy_file_stop rktio cp)
          (thread-pop-kill-callback!)
          (end-atomic)))])))

(define/who (make-file-or-directory-link to path)
  (check who path-string? to)
  (check who path-string? path)
  (define to-path (->path to))
  (define path-host (->host path who '(write)))
  (define to-host (->host/as-is to-path who (host-> path-host)))
  (define r (rktio_make_link rktio path-host to-host (directory-path? to-path)))
  (when (rktio-error? r)
    (raise-filesystem-error who
                            r
                            (format (string-append
                                     "cannot make link~a\n"
                                     "  path: ~a")
                                    (if (racket-error? r RKTIO_ERROR_EXISTS)
                                        ";\n the path already exists"
                                        "")
                                    (host-> path-host)))))

(define (do-resolve-path p who)
  (check who path-string? p)
  (define host-path (->host (path->path-without-trailing-separator (->path p)) who '(exists)))
  (start-atomic)
  (define r0 (rktio_readlink rktio host-path))
  (define r (if (rktio-error? r0)
                r0
                (begin0
                  (rktio_to_bytes r0)
                  (rktio_free r0))))
  (end-atomic)
  (cond
    [(rktio-error? r)
     ;; Errors are not reported, but are treated like non-links
     (define new-path (host-> host-path))
     ;; If cleansing didn't change p, then return an `eq?` path
     (cond
       [(equal? new-path p) p]
       [else new-path])]
    [else (host-> r)]))

(define/who (resolve-path p)
  (do-resolve-path p who))

(module+ for-simplify
  (provide resolve-path-for-simplify)
  (define (resolve-path-for-simplify p)
    (do-resolve-path p 'simplify-path)))

(define/who (expand-user-path p)
  (check who path-string? p)
  (define path (->path p))
  (define bstr (path-bytes path))
  (cond
    [(and (positive? (bytes-length bstr))
          (eqv? (bytes-ref bstr 0) (char->integer #\~)))
     (define host-path (->host/as-is path who #f))
     (start-atomic)
     (define r0 (rktio_expand_user_tilde rktio host-path))
     (define r (if (rktio-error? r0)
                   r0
                   (begin0
                     (rktio_to_bytes r0)
                     (rktio_free r0))))
     (end-atomic)
     (when (rktio-error? r)
       (raise-filesystem-error who
                               r
                               (format (string-append
                                        "bad username in path\n"
                                        "  path: ~a")
                                       (host-> host-path))))
     (host-> r)]
    [else path]))

(define/who (filesystem-root-list)
  (security-guard-check-file who #f '(exists))
  (start-atomic)
  (define r0 (rktio_filesystem_roots rktio))
  (define r (if (rktio-error? r0)
                r0
                (rktio_to_bytes_list r0)))
  (end-atomic)
  (when (rktio-error? r)
    (raise-filesystem-error who r "cannot get roots"))
  (for/list ([p (in-list r)])
    (host-> p)))
