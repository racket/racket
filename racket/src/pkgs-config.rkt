#lang racket/base
(require racket/cmdline
         racket/format
         racket/pretty
         racket/port
         racket/string
         racket/date
         racket/system
         racket/file)

(module+ main
  (command-line
   #:usage-help
   "Used by the top-level Makefile in the main Racket repository."
   "If no <option> is provided, does nothing."
   #:once-any
   [("--pkgs-catalog")
    default-src-catalog src-catalog
    ("Configure a development build to consult first a local catalog,"
     "which is expected to map packages in this repository to directory links,"
     "next <src-catalog>, and then <default-src-catalog> (if different) before"
     "falling back to the built-in default catalog."
     "If a config.rktd file is created, also set its installation-name,"
     "build-stamp, default-scope, interactive-file, and"
     "gui-interactive-file to values suitable for development from Git."
     "If a config.rktd file already exists, check its catalog configuration"
     "and potentially update its build stamp as with `--maybe-update-stamp`.")
    (pkgs-catalog default-src-catalog src-catalog)]
   [("--maybe-update-stamp")
    ("Update the build stamp if and only if a config.rktd file already"
     "exists and indicates that its build stamp was previously generated"
     "by this script.")
    (maybe-update-stamp)]
   [("--display-auto-stamp")
    ("Display to standard output an automatic build stamp (in the same style"
     "as above) followed by a newline. Used by `distro-build`.")
    (displayln (auto-stamp))]
   #:args ()
   (void)))

(define config-dir-path (build-path "racket" "etc"))
(define config-file-path (build-path config-dir-path "config.rktd"))

(define catalog-relative-path (build-path 'up "share" "pkgs-catalog"))
(define catalog-relative-path-str (path->string catalog-relative-path))

(define (write-config config)
  (call-with-atomic-output-file config-file-path
    (λ (out _tmp)
      (pretty-write config out))))

(define (pkgs-catalog default-src-catalog src-catalog)
  (define src-catalog-is-default?
    (equal? src-catalog default-src-catalog))
  (cond
    [(file-exists? config-file-path)
     (define config (file->value config-file-path))
     (define l (hash-ref config 'catalogs #f))
     (define starts-as-expected?
       (and (list? l)
            (pair? l)
            (equal? (car l) catalog-relative-path-str)))
     (define has-src-catalog?
       (or (and src-catalog-is-default?
                (member #f l))
           (member src-catalog l)))
     (unless (and starts-as-expected?
                  has-src-catalog?)
       (error 'pkgs-catalog
              (~a "config file exists, but with a mismatched `catalogs';\n"
                  " the existing configuration does not ~a\n"
                  "  config file: ~a\n"
                  "  expected ~acatalog: ~s\n"
                  "  possible solution: delete the config file")
              (if (not starts-as-expected?)
                  "start as expected"
                  "include the specified catalog")
              config-file-path
              (if (not starts-as-expected?)
                  "initial "
                  "")
              (if (not starts-as-expected?)
                  catalog-relative-path-str
                  src-catalog)))
     (update-stamp-if-auto config)]
    [else
     (printf "Writing ~a\n" config-file-path)
     (make-directory* config-dir-path)
     (write-config (hash 'catalogs
                         (cons catalog-relative-path-str
                               (if src-catalog-is-default?
                                   '(#f)
                                   (list src-catalog #f)))
                         'installation-name
                         "development"
                         'default-scope
                         "installation"
                         'automatic-development-build-stamp?
                         #t
                         'build-stamp
                         (auto-stamp)
                         'interactive-file
                         'racket/interactive
                         'gui-interactive-file
                         'racket/gui/interactive))]))

(define (maybe-update-stamp)
  (when (file-exists? config-file-path)
    (update-stamp-if-auto (file->value config-file-path))))

(define (update-stamp-if-auto config)
  (when (hash-ref config 'automatic-development-build-stamp? #f)
    (printf "Updating build-stamp in ~a\n" config-file-path)
    (write-config (hash-set config 'build-stamp (auto-stamp)))))

(define (auto-stamp)
  (cond
    [(format-current-commit)
     => (λ (desc)
          (~a (today) "-" desc))]
    [else
     (today)]))

(define (today)
  ;; Use Racket instead of:
  ;;  - `date +%Y-%m-%d` to handle SOURCE_DATA_EPOCH and to avoid
  ;;     portability complications.
  ;;  - `git log -1 --pretty=%cs` because the date for snapshot
  ;;     builds should be updated even on days with no commits here.
  ;;     Users can still choose the commit date instead via e.g.
  ;;     `SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)`.
  (define env
    (getenv "SOURCE_DATE_EPOCH"))
  (define date
    (if (and (non-empty-string? env)
             (not (regexp-match? #px"\\D" env)))
        (seconds->date (string->number env) #f)
        (current-date)))
  (parameterize ([date-display-format 'iso-8601])
    (date->string date)))

(define (format-current-commit)
  (let/ec return
    (define git-cmd
      (or (find-executable-path "git")
          (find-executable-path "git.exe")
          (and (eq? 'macosx (system-type))
               (find-executable-path "/opt/local/bin/git"))))
    (unless (and git-cmd
                 (parameterize ([current-output-port (open-output-nowhere)]
                                [current-input-port (open-input-string "")]
                                ;; failure here just means we are not in a git tree,
                                ;; so discard error messages
                                [current-error-port (open-output-nowhere)])
                   (with-handlers ([exn:fail? (λ (e) #f)])
                     (system* git-cmd "rev-parse" "--git-dir"))))
      (return #f))
    (define (git . args)
      ;; failure here means something is likely wrong, so pass through stderr,
      ;; but don't break the build
      (define o (open-output-string))
      (or (and (parameterize ([current-output-port o]
                              [current-input-port (open-input-string "")])
                 (apply system* git-cmd args))
               (let ([s (string-trim (get-output-string o))])
                 (and (non-empty-string? s) s)))
          (return #f)))
    (~a (git "log" "-1" "--abbrev=10" "--pretty=format:%h")
        (or (for/or ([state '("broken" "dirty")]
                     [mark '("-broken" "*")])
              (define desc
                (git "describe" "--always" "--long" (~a "--" state)))
              (and (string-suffix? desc state)
                   mark))
            ""))))
