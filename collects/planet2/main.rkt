#lang racket/base
(require racket/function
         raco/command-name
         "lib.rkt"
         "commands.rkt"
         (prefix-in setup: setup/setup))

(define (setup no-setup? installation? setup-collects)
  (unless (or no-setup?
              (not (member (getenv "PLT_PLANET2_NOSETUP") '(#f ""))))
    (setup:setup
     #:make-user? (not installation?)
     #:collections (and setup-collects
                        (map (lambda (s)
                               (if (list? s) s (list s)))
                             (append setup-collects
                                     (if installation? '("scribblings/main") null)
                                     '("scribblings/main/user")))))))

(define ((pkg-error cmd) . args)
  (apply raise-user-error
         (string->symbol (format "~a ~a" (short-program+command-name) cmd))
         args))

(commands
 "This tool is used for managing installed packages."
 [install
  "Install packages"
  [(#:sym #f) type ("-t") ("Type of <pkg-source>;"
                           "options are: file, dir, file-url, dir-url, github, or name;"
                           "if not specified, the type is inferred syntactically")]
  [(#:str #f) name ("-n") ("Name of package, instead of inferred"
                           "(makes sense only when a single <pkg-source> is given)")]
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool shared ("-s") "Install user-specific packages as shared for all versions"]
  [(#:sym #f) deps ()
   ("Specify the behavior for dependencies;"
    "options are:"
    "  fail: cancels the installation if dependencies are unmet"
    "        (default for most packages)"
    "  force: installs the package despite missing dependencies"
    "  search-ask: looks for the dependencies on your package naming services"
    "              (default if package is an indexed name) and asks if you would"
    "              like it installed"
    "  search-auto: like 'search-ask' but does not ask for permission to install")]
  [#:bool force () "Ignores conflicts"]
  [#:bool ignore-checksums () "Ignores checksums"]
  [#:bool link () ("Link a directory package source in place")]
  #:args pkg-source
  (parameterize ([current-install-system-wide? installation]
                 [current-install-version-specific? (not shared)]
                 [current-pkg-error (pkg-error 'install)])
    (with-package-lock
     (define setup-collects
       (install-cmd #:dep-behavior deps
                    #:force? force
                    #:ignore-checksums? ignore-checksums
                    (for/list ([p (in-list pkg-source)])
                      (pkg-desc p (or (and link 'link) type) name #f))))
     (setup no-setup installation setup-collects)))]
 [update
  "Update packages"
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool shared ("-s") "Operate on the user-specific all-version package database"]
  [#:bool all ("-a") ("Update all packages;"
                      "only if no packages are given on the command line")]
  [(#:sym #f) deps ()
   ("Specify the behavior for dependencies;"
    "options are:"
    "  fail: cancels the installation if dependencies are unmet"
    "        (default for most packages)"
    "  force: installs the package despite missing dependencies"
    "  search-ask: looks for the dependencies on your package naming services"
    "              (default if package is an indexed name) and asks if you would"
    "              like it installed"
    "  search-auto: like 'search-ask' but does not ask for permission to install")]
  [#:bool update-deps () "Check named packages' dependencies for updates"]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation]
                 [current-install-version-specific? (not shared)]
                 [current-pkg-error (pkg-error 'update)])
    (with-package-lock
     (define setup-collects
       (update-packages pkgs
                        #:all? all
                        #:dep-behavior deps
                        #:deps? update-deps))
     (when setup-collects
       (setup no-setup installation setup-collects))))]
 [remove
  "Remove packages"
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool shared ("-s") "Operate on the user-specific all-version package database"]
  [#:bool force () "Force removal of packages"]
  [#:bool auto () "Remove automatically installed packages with no dependencies"]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation]
                 [current-install-version-specific? (not shared)]
                 [current-pkg-error (pkg-error 'remove)])
    (with-package-lock
     (remove-packages pkgs
                      #:auto? auto
                      #:force? force)
     (setup no-setup installation #f)))]
 [show
  "Show information about installed packages"
  [#:bool installation ("-i") "Show only the installation-wide package database"]
  [#:bool shared ("-s") "Show only the user-specific all-version package database"]
  [#:bool user ("-u") "Show only the user- and version-specific package database"]
  #:args ()
  (define only-mode (cond
                     [installation 'i]
                     [shared 's]
                     [user 'u]
                     [else #f]))
  (for ([mode '(i s u)])
    (when (or (eq? mode only-mode) (not only-mode))
      (unless only-mode
        (printf "~a\n" (case mode
                         [(i) "Installation-wide:"]
                         [(s) "User-specific, all-version:"]
                         [(u) "User-spcific, version-specific:"])))
      (parameterize ([current-install-system-wide? (eq? mode 'i)]
                     [current-install-version-specific? (eq? mode 'u)]
                 [current-pkg-error (pkg-error 'show)])
        (with-package-lock
         (show-cmd (if only-mode "" " "))))))]
 [config
  "View and modify the package configuration"
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool shared ("-s") "Operate on the user-specific all-version package database"]
  [#:bool set () "Completely replace the value"]
  #:args key+vals
  (parameterize ([current-install-system-wide? installation]
                 [current-install-version-specific? (not shared)]
                 [current-pkg-error (pkg-error 'config)])
    (with-package-lock
     (config-cmd set key+vals)))]
 [create
  "Bundle a new package"
  [(#:str #f) format ()
   ("Select the format of the package to be created;"
    "options are: zip (the default), tgz, plt")]
  [#:bool manifest () "Creates a manifest file for a directory, rather than an archive"]
  #:args (maybe-dir)
  (parameterize ([current-pkg-error (pkg-error 'create)])
    (create-cmd (if manifest "MANIFEST" (or format "zip")) maybe-dir))])
