#lang racket/base
(require (only-in racket/base [version r:version])
         racket/function
         racket/list
         raco/command-name
         net/url
         "name.rkt"
         "lib.rkt"
         "commands.rkt"
         (prefix-in setup: setup/setup))

(define (setup no-setup? setup-collects)
  (unless (or no-setup?
              (not (member (getenv "PLT_PKG_NOSETUP") '(#f ""))))
    (define installation? (eq? 'installation (current-pkg-scope)))
    (setup:setup
     #:make-user? (not installation?)
     #:avoid-main? (not installation?)
     #:collections (and setup-collects
                        (map (lambda (s)
                               (if (list? s) s (list s)))
                             setup-collects))
     #:tidy? #t
     #:make-doc-index? #t)))

(define ((pkg-error cmd) . args)
  (apply raise-user-error
         (string->symbol (format "~a ~a" (short-program+command-name) cmd))
         args))

(define (call-with-package-scope who given-scope installation shared user thunk)
  (define scope
    (case given-scope
      [(installation user shared) given-scope]
      [else
       (cond
        [installation 'installation]
        [user 'user]
        [shared 'shared]
        [else (default-pkg-scope)])]))
  (parameterize ([current-pkg-scope scope]
                 [current-pkg-error (pkg-error who)])
    (thunk)))

(define (catalog->url s)
  (cond
   [(regexp-match? #rx"^[a-zA-Z]*://" s) (string->url s)]
   [else (path->url (path->complete-path s))]))

(commands
 "This tool is used for managing installed packages."
 [install
  "Install packages"
  #:once-each
  [(#:sym type [file dir file-url dir-url github name] #f) type ("-t") 
   ("Type of <pkg-source>;"
    "valid <types>s are: file, dir, file-url, dir-url, github, or name;"
    "if not specified, the type is inferred syntactically")]
  [(#:str name #f) name ("-n") ("Name of package, instead of inferred"
                                "(makes sense only when a single <pkg-source> is given)")]
  [#:bool no-setup () ("Don't run `raco setup' after changing packages"
                       "(generally not a good idea)")]
  #:once-each
  [(#:sym mode [fail force search-ask search-auto] #f) deps ()
   ("Specify the behavior for dependencies, with <mode> as one of"
    "  fail: cancels the installation if dependencies are unmet"
    "        (default for most packages)"
    "  force: installs the package despite missing dependencies"
    "  search-ask: looks for the dependencies on your package naming services"
    "              (default if package is a package name) and asks if you would"
    "              like it installed"
    "  search-auto: like 'search-ask' but does not ask for permission to install")]
  [#:bool force () "Ignores conflicts"]
  [#:bool ignore-checksums () "Ignores checksums"]
  [#:bool link () ("Link a directory package source in place")]
  [#:bool skip-installed () ("Skip a <pkg-source> if already installed")]
  #:once-any
  [(#:sym scope [installation user shared] #f) scope ()
   ("Select package <scope>, one of"
    "  installation: Install for all users of the Racket installation"
    "  user: Install as user- and version-specific"
    "  shared: Install as user-specific but shared for all Racket versions")]
  [#:bool installation ("-i") "shorthand for `--scope installation'"]
  [#:bool user ("-u") "shorthand for `--scope user'"]
  [#:bool shared ("-s") "shorthand for `--scope shared'"]
  #:once-each
  [(#:str catalog #f) catalog () "Use <catalog> instead of configured catalogs"]
  #:args pkg-source
  (call-with-package-scope
   'install
   scope installation shared user
   (lambda ()
     (unless (or (not name) (package-source->name name))
       ((current-pkg-error) (format "~e is an invalid package name" name)))
     (define setup-collects
       (with-pkg-lock
        (parameterize ([current-pkg-catalogs (and catalog
                                                  (list (catalog->url catalog)))])
          (pkg-install #:dep-behavior deps
                       #:force? force
                       #:ignore-checksums? ignore-checksums
                       #:skip-installed? skip-installed
                       (for/list ([p (in-list pkg-source)])
                         (pkg-desc p (or (and link 'link) type) name #f))))))
     (setup no-setup setup-collects)))]
 [update
  "Update packages"
  #:once-each
  [#:bool no-setup () ("Don't run `raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool all ("-a") ("Update all packages;"
                      "only if no packages are given on the command line")]
  [(#:sym mode [fail force search-ask search-auto] #f) deps ()
   ("Specify the behavior for dependencies, with <mode> as one of"
    "  fail: cancels the installation if dependencies are unmet"
    "        (default for most packages)"
    "  force: installs the package despite missing dependencies"
    "  search-ask: looks for the dependencies on your package naming services"
    "              (default if package is an package name) and asks if you would"
    "              like it installed"
    "  search-auto: like 'search-ask' but does not ask for permission to install")]
  [#:bool update-deps () "Check named packages' dependencies for updates"]
  #:once-any
  [(#:sym scope [installation user shared] #f) scope ()
   ("Select package scope, one of"
    "  installation: Update only for all users of the Racket installation"
    "  user: Update only user- and version-specific packages"
    "  shared: Update only user-specific packages for all Racket versions")]
  [#:bool installation ("-i") "shorthand for `--scope installation'"]
  [#:bool user ("-u") "shorthand for `--scope user'"]
  [#:bool shared ("-s") "shorthand for `--scope shared'"]
  #:args pkg
  (call-with-package-scope
   'update
   scope installation shared user
   (lambda ()
     (define setup-collects
       (with-pkg-lock
        (pkg-update pkg
                    #:all? all
                    #:dep-behavior deps
                    #:deps? update-deps)))
     (setup no-setup setup-collects)))]
 [remove
  "Remove packages"
  #:once-each
  [#:bool no-setup () ("Don't run `raco setup' after changing packages"
                       "(generally not a good idea)")]
  [#:bool force () "Force removal of packages"]
  [#:bool auto () "Remove automatically installed packages with no dependencies"]
  #:once-any
  [(#:sym scope [installation user shared] #f) scope ()
   ("Select package <scope>, one of"
    "  installation: Remove packages for all users of the Racket installation"
    "  user: Remove user- and version-specific packages"
    "  shared: Remove user-specific packages for all Racket versions")]
  [#:bool installation ("-i") "shorthand for `--scope installation'"]
  [#:bool user ("-u") "shorthand for `--scope user'"]
  [#:bool shared ("-s") "shorthand for `--scope shared'"]
  #:args pkg
  (call-with-package-scope
   'remove
   scope installation shared user
   (lambda ()
     (define setup-collects
       (with-pkg-lock
        (pkg-remove pkg
                    #:auto? auto
                    #:force? force)))
     (setup no-setup setup-collects)))]
 [show
  "Show information about installed packages"
  #:once-each
  [#:bool dir ("-d") "Show the directory where the package is installed"]
  #:once-any
  [(#:sym scope [installation user shared] #f) scope ()
   ("Show only for package <scope>, one of"
    "  installation: Show only for all users of the Racket installation"
    "  user: Show only user- and version-specific"
    "  shared: Show only user-specific for all Racket versions")]
  [(#:str vers #f) version ("-v") "Show only user-specific for Racket <vers>"]
  [#:bool installation ("-i") "shorthand for `--scope installation'"]
  [#:bool user ("-u") "shorthand for `--scope user'"]
  [#:bool shared ("-s") "shorthand for `--scope shared'"]
  #:args ()
  (define only-mode (case scope
                      [(installation user shared) scope]
                      [else
                       (cond
                        [installation 'installation]
                        [shared 'shared]
                        [user 'user]
                        [else (if version 'user #f)])]))
  (for ([mode '(installation shared user)])
    (when (or (eq? mode only-mode) (not only-mode))
      (unless only-mode
        (printf "~a\n" (case mode
                         [(installation) "Installation-wide:"]
                         [(shared) "User-specific, all-version:"]
                         [(user) (format "User-specific, version-specific (~a):"
                                         (or version (r:version)))])))
      (parameterize ([current-pkg-scope mode]
                     [current-pkg-error (pkg-error 'show)]
                     [current-pkg-scope-version (or version (r:version))])
        (with-pkg-lock/read-only
         (pkg-show (if only-mode "" " ") #:directory? dir)))))]
 [create
  "Bundle a package from a directory or installed package"
  #:once-any
  [#:bool from-dir () "Treat <directory-or-package> as a directory (the default)"]
  [#:bool from-install () "Treat <directory-or-package> as a package name"]
  #:once-any
  [(#:sym fmt [zip tgz plt] #f) format ()
   ("Select the format of the package to be created;"
    "valid <fmt>s are: zip (the default), tgz, plt")]
  [#:bool manifest () "Creates a manifest file for a directory, rather than an archive"]
  #:once-any
  [#:bool as-is () "Bundle the directory/package as-is (the default)"]
  [#:bool source () "Bundle sources only"]
  [#:bool binary () "Bundle bytecode and rendered documentation without sources"]
  [#:bool built () "Bundle sources, bytecode and rendered documentation"]
  #:once-each
  [(#:str dest-dir #f) dest () "Create output files in <dest-dir>"]
  #:args (directory-or-package)
  (parameterize ([current-pkg-error (pkg-error 'create)])
    (pkg-create (if manifest 'MANIFEST (or format 'zip)) 
                directory-or-package
                #:dest (and dest 
                            (path->complete-path dest))
                #:source (cond
                          [from-install 'name]
                          [else 'dir])
                #:mode (cond
                        [source 'source]
                        [binary 'binary]
                        [built 'built]
                        [else 'as-is])))]
 [config
  "View and modify the package manager's configuration"
  #:once-each
  [#:bool set () "Completely replace the value"]
  #:once-any
  [(#:sym scope [installation user shared] #f) scope ()
   ("Select configuration <scope>, one of"
    "  installation: Operate on the installation-wide package configuration"
    "  user: Operate on the user-specific, version-specific package configuration"
    "  shared: Operate on the user-specific all-version package configuration")]
  [#:bool installation ("-i") "shorthand for `--scope installation'"]
  [#:bool user ("-u") "shorthand for `--scope user'"]
  [#:bool shared ("-s") "shorthand for `--scope shared'"]
  #:args key/val
  (call-with-package-scope
   'config
   scope installation shared user
   (lambda ()
     (if set
         (with-pkg-lock
          (pkg-config #t key/val))
         (with-pkg-lock/read-only
          (pkg-config #f key/val)))))]
 [catalog-show
  "Show information about packages as reported by catalog"
  #:once-any 
  [(#:str catalog #f) catalog () "Use <catalog> instead of configured catalogs"]
  #:once-each
  [#:bool all () "Show all packages"]
  [#:bool only-names () "Show only package names"]
  [#:bool modules () "Show implemented modules"]
  #:args pkg-name
  (when (and all (pair? pkg-name))
    ((pkg-error 'catalog-show) "both `--all' and package names provided"))
  (parameterize ([current-pkg-catalogs (and catalog
                                            (list (catalog->url catalog)))]
                 [current-pkg-error (pkg-error 'catalog-show)])
    (pkg-catalog-show pkg-name 
                      #:all? all
                      #:only-names? only-names
                      #:modules? modules))]
 [catalog-copy
  "Copy/merge package name catalogs"
  #:once-each
  [#:bool from-config () "Include currently configured catalogs last"]
  #:once-any
  [#:bool force () "Force replacement fo existing file/directory"]
  [#:bool merge () "Merge to existing database"]
  #:once-each
  [#:bool override () "While merging, override existing with new"]
  #:args catalog
  (parameterize ([current-pkg-error (pkg-error 'catalog-copy)])
    (when (null? catalog)
      ((current-pkg-error) "need a destination catalog"))
    (pkg-catalog-copy (drop-right catalog 1)
                      (last catalog)
                      #:from-config? from-config
                      #:force? force
                      #:merge? merge
                      #:override? override))])
