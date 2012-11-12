#lang racket/base
(require racket/function
         racket/system
         "lib.rkt"
         "commands.rkt")

(define (setup dont-setup)
  (unless (or dont-setup
              (equal? "1" (getenv "PLT_PLANET2_DONTSETUP")))
    (system "raco setup")))

(commands
 "This tool is used for managing installed packages."
 [install
  "Install packages"
  [#:bool dont-setup () "Don't run 'raco setup' after changing packages (generally not a good idea)"]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [(#:sym #f) deps ()
   ("Specify the behavior for dependencies."
    "Options are: fail, force, search-ask, search-auto."
    "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
    "  'force' installs the package despite missing dependencies."
    "  'search-ask' looks for the dependencies on your package naming services (default if package is an indexed name) and asks if you would like it installed."
    "  'search-auto' is like 'search-ask' but does not ask for permission to install.")]
  [#:bool force () "Ignores conflicts"]
  [#:bool ignore-checksums () "Ignores checksums"]
  [#:bool link () "When used with a directory package, leave the directory in place, but add a link to it in the package directory. This is a global setting for all installs for this command, which means it affects dependencies... so make sure the dependencies exist first."]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (install-cmd #:dep-behavior deps
                  #:force? force
                  #:link? link
                  #:ignore-checksums? ignore-checksums
                  (map (curry cons #f) pkgs))
     (setup dont-setup)))]
 [update
  "Update packages"
  [#:bool dont-setup () "Don't run 'raco setup' after changing packages (generally not a good idea)"]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool all ("-a") "Update all packages (if no packages are given on the command line)"]
  [(#:sym #f) deps ()
   ("Specify the behavior for dependencies."
    "Options are: fail, force, search-ask, search-auto."
    "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
    "  'force' installs the package despite missing dependencies."
    "  'search-ask' looks for the dependencies on your package naming services (default if package is an indexed name) and asks if you would like it installed."
    "  'search-auto' is like 'search-ask' but does not ask for permission to install.")]
  [#:bool update-deps () "Check named packages' dependencies for updates"]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (update-packages pkgs
                      #:all? all
                      #:dep-behavior deps
                      #:deps? update-deps)
     (setup dont-setup)))]
 [remove
  "Remove packages"
  [#:bool dont-setup () "Don't run 'raco setup' after changing packages (generally not a good idea)"]
  [#:bool installation ("-i") "Operate on the installation-wide package database"]
  [#:bool force () "Force removal of packages"]
  [#:bool auto () "Remove automatically installed packages with no dependencies"]
  #:args pkgs
  (parameterize ([current-install-system-wide? installation])
    (with-package-lock
     (remove-packages pkgs
                      #:auto? auto
                      #:force? force)
     (setup dont-setup)))]
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
  [(#:str "plt") format ()
   ("Select the format of the package to be created."
    "Options are: tgz, zip, plt")]
  [#:bool manifest () "Creates a manifest file for a directory, rather than an archive"]
  #:args (maybe-dir)
  (create-cmd (if manifest "MANIFEST" format) maybe-dir)])
