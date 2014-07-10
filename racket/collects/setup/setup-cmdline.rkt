#lang racket/base

;; Command-line parsing is in its own module because it has to be used
;;  both in setup.ss (pre-zo, pre-cm) and setup-go.rkt (use zos and cm).
;; This means that command lines will be parsed twice.

(require racket/cmdline
         raco/command-name
         pkg/name
         "private/command-name.rkt")

(provide parse-cmdline)

;; The result of parse-cmdline is three lists:
;;  - An assoc list mapping flag symbols to booleans
;;     (nearly all symbols correspond to parameter names
;;      in setup-go.rkt)
;;  - A list of specific collections
;;  - A list of archives

(define (parse-cmdline argv)

  (define x-specific-planet-packages '())
  (define x-flags null)
  (define (add-flags l)
    (set! x-flags (append (reverse l) x-flags)))

  (define-values (short-name long-name raco?) (get-names))

  ;; Beware of the poor-man's duplicate of this command-line specification
  ;; in "main.rkt"!
  (define-values (x-specific-collections x-specific-packages x-archives)
    (command-line
     #:program long-name
     #:argv argv
     #:help-labels
     " --------------------------- collections --------------------------- "
     " If no collection, package, or archive is specified, all are setup"
     #:once-each
     [("--only") "Set up only specified, even if none"
      (add-flags '((make-only #t)))]
     [("-l") => (lambda (flag . collections)
                  (check-collections short-name collections)
                  (cons 'collections (map list collections)))
             '("Setup specified <collection>s" "collection")]
     [("--pkgs") => (lambda (flag . pkgs)
                      (check-packages short-name pkgs)
                      (cons 'packages pkgs))
             '("Setup collections in specified <pkg>s" "pkg")]
     #:multi
     [("-P") owner package-name maj min
      "Setup specified PLaneT packages"
      (set! x-specific-planet-packages (cons (list owner package-name maj min)
                                             x-specific-planet-packages))]
     #:once-each
     [("--doc-index") "Rebuild documentation index along with specified"
      (add-flags '((make-doc-index #t)))]
     [("--tidy") "Clear references to removed items outside of specified"
      (add-flags '((make-tidy #t)))]
     #:help-labels
     " ------------------------------ tasks ------------------------------ "
     #:once-each
     [("-c" "--clean") "Delete existing compiled files; implies -nxiD"
      (add-flags '((clean #t)
                   (make-zo #f)
                   (call-install #f)
                   (make-launchers #f)
                   (make-info-domain #f)
                   (make-docs #f)))]
     [("-n" "--no-zo") "Do not create \".zo\" files"
      (add-flags '((make-zo #f)))]
     [("--trust-zos") "Trust existing \".zo\"s (use only with prepackaged \".zo\"s)"
      (add-flags '((trust-existing-zos #t)))]
     [("-x" "--no-launcher") "Do not produce launcher programs"
      (add-flags '((make-launchers #f)))]
     [("-F" "--no-foreign-libs") "Do not install foreign libraries"
      (add-flags '((make-foreign-libs #f)))]
     [("-i" "--no-install") "Do not call collection-specific pre-installers"
      (add-flags '((call-install #f)))]
     [("-I" "--no-post-install") "Do not call collection-specific post-installers"
      (add-flags '((call-post-install #f)))]
     [("-d" "--no-info-domain") "Do not produce info-domain caches"
      (add-flags '((make-info-domain #f)))]
     [("-D" "--no-docs") "Do not compile .scrbl files and do not build documentation"
      (add-flags '((make-docs #f)))]
     [("--doc-pdf") dir "Build documentation PDFs, write to <dir>"
      (add-flags `((doc-pdf-dest ,dir)))]
     [("-K" "--no-pkg-deps") "Do not check package dependencies"
      (add-flags '((check-dependencies #f)))]
     [("--check-pkg-deps") "Check package dependencies when collections specified"
      (add-flags '((always-check-dependencies #t)))]
     [("--fix-pkg-deps") "Auto-repair package-dependency declarations"
      (add-flags '((check-dependencies #t)
                   (fix-dependencies #t)))]
     [("--unused-pkg-deps") "Check for unused package-dependency declarations"
      (add-flags '((check-dependencies #t)
                   (check-unused-dependencies #t)))]
     #:help-labels
     " ------------------------------ users ------------------------------ "
     #:once-each
     [("-U" "--no-user") "Do not setup user-specific collections (implies --no-planet)"
      (add-flags '((make-user #f) (make-planet #f)))]
     [("--no-planet") "Do not setup PLaneT packages"
      (add-flags '((make-planet #f)))]
     [("--avoid-main") "Do not make main-installation files"
      (add-flags '((avoid-main-installation #t)))]
     #:help-labels
     " ------------------------------ modes ------------------------------ "
     #:once-each
     [("-j" "--jobs" "--workers") n "Use <n> parallel jobs"
      (add-flags `((parallel-workers ,(string->number n))))]
     [("-v" "--verbose") "See names of compiled files and info printfs"
      (add-flags '((verbose #t)))]
     [("-m" "--make-verbose") "See make and compiler usual messages"
      (add-flags '((make-verbose #t)))]
     [("-r" "--compile-verbose") "See make and compiler verbose messages"
      (add-flags '((make-verbose #t)
                   (compiler-verbose #t)))]
     [("--mode") mode "Select a compilation mode, such as \"errortrace\""
      (add-flags `((compile-mode ,mode)))]
     [("--fail-fast") "Trigger a break on the first error"
      (add-flags '((fail-fast #t)))]
     [("-p" "--pause") "Pause at the end if there are any errors"
      (add-flags '((pause-on-errors #t)))]
     #:help-labels
     " ---------------------------- archives ----------------------------- "
     #:once-each
     [("-A") => (λ (flag . archives)
                  (cons 'archives archives))
             '("Unpack and install <archive>s" "archive")]
     [("--force") "Treat version mismatches for archives as mere warnings"
      (add-flags '((force-unpacks #t)))]
     [("-a" "--all-users") "Install archives to main (not user-specific) installation"
      (add-flags '((all-users #t)))]
     #:help-labels
     " ------------------------------ misc ------------------------------- "

     
     #:handlers
     (lambda (collections/pkgs/archives . rest)
       (define (get key)
         (if (and (pair? collections/pkgs/archives)
                  (eq? (caar collections/pkgs/archives) key))
             (cdr (car collections/pkgs/archives))
             '()))
       (let ([pre-archives (get 'archives)]
             [pre-collections (get 'collections)]
             [pre-packages (get 'packages)])
         (cond
           [raco?
            (check-collections short-name rest)
            (values (append pre-collections (map list rest))
                    pre-packages
                    pre-archives)]
           [else
            (values pre-collections
                    pre-packages
                    (append pre-archives rest))])))
     (if raco? '("collection") '("archive"))
     (lambda (s)
       (display s)
       (exit 0))))

    (values short-name x-flags 
            x-specific-collections x-specific-packages x-specific-planet-packages
            x-archives))

(define (check-collections name collections)
  (for ((v (in-list collections)))
    ;; A normal-form collection path matches a symbolic module path;
    ;; this is a bit of a hack, but it's not entirely a coincidence:
    (unless (module-path? (string->symbol v))
      (raise-user-error (string->symbol name)
                        "bad collection path~a: ~a"
                        (cond [(regexp-match? #rx"/$" v)
                               " (trailing slash not allowed)"]
                              [(regexp-match? #rx"\\\\" v)
                               " (backslash not allowed)"]
                              [else ""])
                        v))))

(define (check-packages name packages)
  (for ((v (in-list packages)))
    (define-values (n type) (package-source->name+type v #f))
    (unless (and (eq? type 'name)
                 (equal? n v))
      (raise-user-error (string->symbol name)
                        "bad package name: ~a"
                        v))))
