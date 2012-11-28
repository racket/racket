#lang racket/base
(require racket/function
         "lib.rkt"
         "commands.rkt"
         (prefix-in setup: setup/setup))

(define (setup no-setup)
  (unless (or no-setup
              (equal? "1" (getenv "PLT_PLANET2_NOSETUP")))
    (setup:setup)))

(commands
 "This tool is used for managing installed packages."
 [install
  "Install packages"
  [(#:sym #f) type ("-t") ("Type of <pkg-source>;"
                           "options are: file, dir, url, github, or name;"
                           "if not specified, the type is inferred syntactically")]
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
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
  [#:bool link () ("Link a directory package source in place;"
                   "this is a global setting for all installs for this command, which means"
                   "that it affects dependencies... so make sure the dependencies exist first")]
  #:args pkg-source
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (install-cmd #:dep-behavior deps
                  #:force? force
                  #:link? link
                  #:ignore-checksums? ignore-checksums
                  #:type (or type 
                             (and link 'dir))
                  (map (curry cons #f) pkg-source))
     (setup no-setup)))]
 [update
  "Update packages"
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
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
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (when (update-packages pkgs
                            #:all? all
                            #:dep-behavior deps
                            #:deps? update-deps)
       (setup no-setup))))]
 [remove
  "Remove packages"
  [#:bool no-setup () ("Don't run 'raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool force () "Force removal of packages"]
  [#:bool auto () "Remove automatically installed packages with no dependencies"]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (remove-packages pkgs
                      #:auto? auto
                      #:force? force)
     (setup no-setup)))]
 [show
  "Show information about installed packages"
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  #:args ()
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (show-cmd)))]
 [config
  "View and modify the package configuration"
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool set () "Completely replace the value"]
  #:args key+vals
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (config-cmd set key+vals)))]
 [create
  "Bundle a new package"
  [(#:str #f) format ()
   ("Select the format of the package to be created;"
    "options are: tgz, zip, plt")]
  [#:bool manifest () "Creates a manifest file for a directory, rather than an archive"]
  #:args (maybe-dir)
  (unless (or manifest format)
    (error 'planet2 "You must specify an archive format"))
  (create-cmd (if manifest "MANIFEST" format) maybe-dir)])
