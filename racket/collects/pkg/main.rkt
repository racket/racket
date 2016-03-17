#lang racket/base
(require racket/function
         racket/list
         racket/format
         racket/path
         racket/splicing
         raco/command-name
         setup/dirs
         net/url
         "name.rkt"
         "lib.rkt"
         "commands.rkt"
         (prefix-in setup: setup/setup)
         (for-syntax racket/base
                     syntax/strip-context))

(define (setup what no-setup? fail-fast? setup-collects jobs)
  (unless (or (eq? setup-collects 'skip)
              no-setup?
              (not (member (getenv "PLT_PKG_NOSETUP") '(#f ""))))
    (define installation? (eq? 'installation (current-pkg-scope)))
    (unless (setup:setup
             #:make-user? (not installation?)
             #:avoid-main? (not installation?)
             #:collections (and setup-collects
                                (map (lambda (s)
                                       (if (list? s) s (list s)))
                                     setup-collects))
             #:tidy? #t
             #:make-doc-index? #t
             #:jobs jobs
             #:fail-fast? fail-fast?)
      ((current-pkg-error)
       "packages ~a, although setup reported errors"
       what))))

(define ((pkg-error cmd) . args)
  (apply raise-user-error
         (string->symbol (format "~a ~a" (short-program+command-name) cmd))
         args))

;; Selects scope from `given-scope' through `user' arguments, or infers
;; a scope from `pkgs' if non-#f, and then calls `thunk'.
(define (call-with-package-scope who given-scope scope-dir installation user pkgs
                                 pkgs-type clone-type-can-be-name? given-name
                                 thunk)
  (define scope
    (case given-scope
      [(installation user) given-scope]
      [else
       (cond
        [installation 'installation]
        [user 'user]
        [(path-string? given-scope) 
         ;; This can happens when a #:scope value is given a path programmatically.
         ;; Make it easier on clients by alloing that.
         (path->complete-path given-scope)]
        [scope-dir (path->complete-path scope-dir)]
        [else
         (define default-scope (default-pkg-scope))
         (or (and pkgs
                  ;; Infer a scope from given package names:
                  (parameterize ([current-pkg-scope 'user]
                                 [current-pkg-error (pkg-error who)])
                    (with-pkg-lock/read-only
                     (define-values (pkg scope)
                       (for/fold ([prev-pkg #f] [prev-scope #f]) ([pkg (in-list pkgs)])
                         (define-values (pkg-name pkg-type/unused)
                           (cond
                            [given-name (values given-name #f)]
                            [(and (eq? pkgs-type 'clone)
                                  clone-type-can-be-name?
                                  (let-values ([(pkg-name pkg-type) 
                                                (package-source->name+type pkg #f)])
                                    (and (eq? pkg-type 'name)
                                         pkg-name)))
                             => (lambda (name)
                                  (values name #f))]
                            [else
                             (package-source->name+type pkg pkgs-type
                                                        #:must-infer-name? #t
                                                        #:complain
                                                        (lambda (s msg)
                                                          ((current-pkg-error) 
                                                           (~a "~a\n"
                                                               "  given: ~a")
                                                           msg s)))]))
                         (define scope (find-pkg-installation-scope pkg-name))
                         (cond
                          [(or (not prev-pkg) (not prev-scope)) (values pkg scope)]
                          [(not scope) (values prev-pkg prev-scope)]
                          [(equal? scope prev-scope) (values prev-pkg prev-scope)]
                          [else
                           ((current-pkg-error)
                            (~a "given packages are installed in different scopes\n"
                                "  package: ~a\n"
                                "  scope: ~a\n"
                                "  second package: ~a\n"
                                "  second scope: ~a")
                            prev-pkg
                            prev-scope
                            pkg
                            scope)])))
                     (when (and scope
                                (not (equal? scope default-scope)))
                       (printf "Inferred package scope: ~a\n" scope))
                     scope)))
             ;; No inference, so use configured default scope:
             default-scope)])]))
  (parameterize ([current-pkg-scope scope]
                 [current-pkg-error (pkg-error who)])
    (thunk)))

(define (catalog->url s)
  (cond
   [(regexp-match? #rx"^[a-zA-Z]*://" s) (string->url s)]
   [else (path->url (path->complete-path s))]))

(define (clone-to-package-name clone cmd)
  ;; Use directory name as sole package name, if possible
  (define-values (base name dir?) (split-path clone))
  (cond
   [(and (path? name)
         (let-values ([(pkg-name pkg-type) 
                       (package-source->name+type (path-element->string name) #f)])
           (eq? pkg-type 'name)))
    (define pkg (path-element->string name))
    (printf (~a "Inferred package name from given `--clone' path\n"
                "  package: ~a\n"
                "  given path: ~a\n")
            pkg
            clone)
    (list pkg)]
   [else
    ((pkg-error cmd)
     (~a "cannot extract a valid package name from the `--clone' path\n"
         "  given path: ~a")
     clone)]))

  (define-syntax (make-commands stx)
    (syntax-case stx ()
      [(_ #:scope-flags (scope-flags ...)
          #:dry-run-flags (dry-run-flags ...)
          #:job-flags (job-flags ...)
          #:trash-flags (trash-flags ...)
          #:catalog-flags (catalog-flags ...)
          #:install-type-flags (install-type-flags ...)
          #:install-dep-flags ((install-dep-flags ... (dep-desc ...)))
          #:install-dep-desc (install-dep-desc ...)
          #:install-force-flags (install-force-flags ...)
          #:install-clone-flags (install-clone-flags ...)
          #:update-deps-flags (update-deps-flags ...)
          #:install-copy-flags (install-copy-flags ...)
          #:install-copy-defns (install-copy-defns ...)
          #:install-copy-checks (install-copy-checks ...))
       (replace-context
        stx
         #`(commands
            "This tool is used for managing installed packages."
            "pkg-~a-command"
            ;; ----------------------------------------
            [install
             "Install packages"
             #:usage-help "Installs the packages specified by <pkg-source> ..., and"
                          "if no sources are specified, installs the current directory"
             #:once-each
             install-type-flags ...
             #:once-any
             [install-dep-flags ...
                                (dep-desc ... 
                                          install-dep-desc ...)]
             [#:bool auto () "Shorthand for `--deps search-auto'"]
             #:once-each
             update-deps-flags ...
             #:once-any
             install-copy-flags ...
             #:once-any
             scope-flags ...
             #:once-each
             catalog-flags ...
             [#:bool skip-installed () ("Skip a <pkg-source> if already installed")]
             [#:bool pkgs () ("Install only the specified packages, even when none are provided")]
             install-force-flags ...
             install-clone-flags ...
             dry-run-flags ...
             job-flags ...
             trash-flags ...
             [#:bool fail-fast () ("Break `raco setup' when it discovers an error")]
             #:args pkg-source
             install-copy-defns ...
             (let ([pkg-source
                    ;; Implement special rules for an empty list of package sources
                    (cond
                     [(not (null? pkg-source))
                      pkg-source]
                     [clone
                      (clone-to-package-name clone 'install)]
                     [else
                      pkg-source])])
               (call-with-package-scope
                'install
                scope scope-dir installation user #f a-type #f name
                (lambda ()
                  install-copy-checks ...
                  (when (and name (> (length pkg-source) 1))
                    ((current-pkg-error) (format "the --name flag only makes sense with a single package source")))
                  (unless (or (not name) (package-source->name name))
                    ((current-pkg-error) (format "~e is an invalid package name" name)))
                  ;; if no sources were supplied, and `--pkgs` was not
                  ;; explicitly specified, install the current directory
                  ;; as a linked directory
                  (define-values (sources a-type*)
                    (if (and (not pkgs) (null? pkg-source))
                        (begin
                          (printf "Linking current directory as a package\n")
                          (values (list (path->string (current-directory)))
                                  'link))
                        (values pkg-source a-type)))
                  (define setup-collects
                    (with-pkg-lock
                        (parameterize ([current-pkg-catalogs (and catalog
                                                                  (list (catalog->url catalog)))])
                          (pkg-install #:from-command-line? #t
                                       #:dep-behavior (or (and auto 'search-auto)
                                                          deps
                                                          (cond
                                                           [batch 'fail]                                                           
                                                           [else 'search-ask]))
                                       #:all-platforms? all-platforms
                                       #:force? force
                                       #:ignore-checksums? ignore-checksums
                                       #:strict-doc-conflicts? strict-doc-conflicts
                                       #:use-cache? (not no-cache)
                                       #:skip-installed? skip-installed
                                       #:update-deps? update-deps
                                       #:update-implies? (not ignore-implies)
                                       #:strip (or (and source 'source)
                                                   (and binary 'binary)
                                                   (and binary-lib 'binary-lib))
                                       #:force-strip? force
                                       #:multi-clone-behavior (or multi-clone 
                                                                  (if batch
                                                                      'fail
                                                                      'ask))
                                       #:pull-behavior pull
                                       #:link-dirs? link-dirs?
                                       #:dry-run? dry-run
                                       #:use-trash? (not no-trash)
                                       (for/list ([p (in-list sources)])
                                         (pkg-desc p a-type* name checksum #f
                                                   #:path (and (eq? a-type* 'clone)
                                                               (path->complete-path clone))))))))
                  (setup "installed" no-setup fail-fast setup-collects jobs))))]
            ;; ----------------------------------------
            [update
             "Update packages"
             #:once-each
             [#:bool all ("-a") ("Update all packages if no <pkg-source> is given")]
             [#:bool lookup ()
                     ("When <pkg-source> is a name, get source from a catalog instead of"
                      "  using the currently installed source; unclones or combines with `--clone'")]
             #:once-each
             install-type-flags ...
             #:once-any
             [install-dep-flags ...
                                (dep-desc ... 
                                          install-dep-desc ...)]
             [#:bool auto () "Shorthand for `--deps search-auto' plus `--update-deps'"]
             #:once-each
             update-deps-flags ...
             #:once-any
             install-copy-flags ...
             #:once-any
             scope-flags ...
             #:once-each
             catalog-flags ...
             [#:bool skip-uninstalled () ("Skip a given <pkg-source> if not installed")]
             install-force-flags ...
             install-clone-flags ...
             dry-run-flags ...
             job-flags ...
             trash-flags ...
             #:args pkg-source
             install-copy-defns ...
             (let ([pkg-source
                    ;; Implement special rules for an empty list of package sources
                    (cond
                     [(or (not (null? pkg-source))
                          all) ; --all has is own treatment of an empty list
                      pkg-source]
                     [clone
                      (clone-to-package-name clone 'update)]
                     [else
                      ;; In a package directory?
                      (define pkg (path->pkg (current-directory)))
                      (if pkg
                          (begin
                            (printf "Updating current directory's package: ~a\n"
                                    pkg)
                            (list pkg))
                          null)])])
               (call-with-package-scope
                'update
                scope scope-dir installation user pkg-source a-type #t name
                (lambda ()
                  install-copy-checks ...
                  (define clone-path (and (eq? a-type 'clone)
                                          (path->complete-path clone)))
                  (define setup-collects
                    (with-pkg-lock
                        (parameterize ([current-pkg-catalogs (and catalog
                                                                  (list (catalog->url catalog)))])
                          (pkg-update (for/list ([pkg-source (in-list pkg-source)])
                                        (cond
                                         [lookup
                                          (pkg-desc pkg-source a-type name checksum #f
                                                    #:path clone-path)]
                                         [else
                                          (define-values (pkg-name pkg-type) 
                                            (package-source->name+type pkg-source a-type))
                                          (if (eq? pkg-type 'name)
                                              pkg-name
                                              (pkg-desc pkg-source a-type name checksum #f
                                                        #:path clone-path))]))
                                      #:from-command-line? #t
                                      #:all? all
                                      #:dep-behavior (or (and auto 'search-auto)
                                                         deps
                                                         (cond
                                                          [batch 'fail]
                                                          [else 'search-ask]))
                                      #:all-platforms? all-platforms
                                      #:force? force
                                      #:ignore-checksums? ignore-checksums
                                      #:strict-doc-conflicts? strict-doc-conflicts
                                      #:use-cache? (not no-cache)
                                      #:skip-uninstalled? skip-uninstalled
                                      #:update-deps? (or update-deps auto)
                                      #:update-implies? (not ignore-implies)
                                      #:strip (or (and source 'source)
                                                  (and binary 'binary)
                                                  (and binary-lib 'binary-lib))
                                      #:force-strip? force
                                      #:lookup-for-clone? lookup
                                      #:multi-clone-behavior (or multi-clone
                                                                 (if batch
                                                                     'fail
                                                                     'ask))
                                      #:pull-behavior pull
                                      #:link-dirs? link-dirs?
                                      #:infer-clone-from-dir? (not (or link static-link copy))
                                      #:dry-run? dry-run
                                      #:use-trash? (not no-trash)))))
                  (setup "updated" no-setup #f setup-collects jobs))))]
            ;; ----------------------------------------
            [remove
             "Remove packages"
             #:once-each
             [#:bool demote () "Demote to auto-installed, instead of removing"]
             [#:bool force () "Remove even if package has dependents"]
             [#:bool auto () "Also remove auto-installed packages that have no dependents"]
             #:once-any
             scope-flags ...
             #:once-each
             dry-run-flags ...
             job-flags ...
             trash-flags ...
             #:args pkg
             (call-with-package-scope
              'remove
              scope scope-dir installation user pkg 'name #f #f
              (lambda ()
                (define setup-collects
                  (with-pkg-lock
                   (pkg-remove pkg
                               #:from-command-line? #t
                               #:demote? demote
                               #:auto? auto
                               #:force? force
                               #:dry-run? dry-run
                               #:use-trash? (not no-trash))))
                (setup "removed" no-setup #f setup-collects jobs)))]
            ;; ----------------------------------------
            [new
             "Populate a new directory with the stubs of a package"
             #:args (pkg)
             (parameterize ([current-pkg-error (pkg-error 'new)])
               (pkg-new pkg))]
            ;; ----------------------------------------
            [show
             "Show information about installed packages"
             #:usage-help
             "Set the COLUMNS environment variable to configure the output without `-l'."
             #:once-each
             [#:bool all ("-a") "Show auto-installed packages, too"]
             [#:bool long ("-l") "Show full column content"]
             [#:bool full-checksum () "Show the full checksum"]
             [#:bool rx () "Treat <pkgs> as regular expressions"]
             [#:bool dir ("-d") "Show the directory where the package is installed"]
             #:once-any
             scope-flags ...
             [(#:str vers #f) version ("-v") "Show user-specific for installation <vers>"]
             #:args pkg
             (define only-mode (case scope
                                 [(installation user) scope]
                                 [else
                                  (cond
                                   [scope-dir (path->complete-path scope-dir)]
                                   [installation 'installation]
                                   [user 'user]
                                   [else (if version 'user #f)])]))
             (define pkgs* (if (pair? pkg) pkg #f))
             (for ([mode (if only-mode
                             (list only-mode)
                             (append (let ([main (find-pkgs-dir)])
                                       (reverse
                                        (for/list ([d (get-pkgs-search-dirs)])
                                          (if (equal? d main)
                                              'installation
                                              (simple-form-path d)))))
                                     '(user)))])
               (when (or (equal? mode only-mode) (not only-mode))
                 (unless only-mode
                   (printf "~a\n" (case mode
                                    [(installation) "Installation-wide:"]
                                    [(user) (format "User-specific for installation ~s:"
                                                    (or version (get-installation-name)))]
                                    [else (format "~a:" mode)])))
                 (parameterize ([current-pkg-scope mode]
                                [current-pkg-error (pkg-error 'show)]
                                [current-pkg-scope-version (or version (get-installation-name))])
                   (with-pkg-lock/read-only
                    (pkg-show (if only-mode "" " ") pkgs*
                              #:auto? all
                              #:long? long
                              #:rx? rx
                              #:full-checksum? full-checksum
                              #:directory? dir)))))]
            ;; ----------------------------------------
            [migrate
             "Install packages installed for other version/name"
             #:once-each
             [install-dep-flags ...
                                (dep-desc ...
                                          "where the default is `search-auto'")]
             #:once-any
             [#:bool source () ("Strip built elements of the package before installing")]
             [#:bool binary () ("Strip source elements of the package before installing")]
             [#:bool binary-lib () ("Strip source elements and documentation before installing")]
             #:once-any
             scope-flags ...
             #:once-each
             catalog-flags ...
             install-force-flags ...
             dry-run-flags ...
             job-flags ...
             #:args (from-version)
             (call-with-package-scope
              'migrate
              scope scope-dir installation user #f #f #f #f
              (lambda ()
                (define setup-collects
                  (with-pkg-lock
                   (parameterize ([current-pkg-catalogs (and catalog
                                                             (list (catalog->url catalog)))])
                     (pkg-migrate from-version
                                  #:from-command-line? #t
                                  #:dep-behavior deps
                                  #:force? force
                                  #:all-platforms? all-platforms
                                  #:ignore-checksums? ignore-checksums
                                  #:strict-doc-conflicts? strict-doc-conflicts
                                  #:use-cache? (not no-cache)
                                  #:strip (or (and source 'source)
                                              (and binary 'binary)
                                              (and binary-lib 'binary-lib))
                                  #:force-strip? force
                                  #:dry-run? dry-run))))
                (setup "migrated" no-setup #f setup-collects jobs)))]
            ;; ----------------------------------------
            [create
             "Bundle package from a directory or installed package"
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
             [#:bool binary-lib () "Bundle bytecode without sources or documentation"]
             [#:bool built () "Bundle sources, bytecode and rendered documentation"]
             #:once-each
             [(#:str dest-dir #f) dest () "Create output files in <dest-dir>"]
             #:args (directory-or-package)
             (parameterize ([current-pkg-error (pkg-error 'create)])
               (pkg-create (if manifest 'MANIFEST (or format 'zip)) 
                           directory-or-package
                           #:from-command-line? #t
                           #:dest (and dest 
                                       (path->complete-path dest))
                           #:source (cond
                                     [from-install 'name]
                                     [else 'dir])
                           #:mode (cond
                                   [source 'source]
                                   [binary 'binary]
                                   [binary-lib 'binary-lib]
                                   [built 'built]
                                   [else 'as-is])))]
            ;; ----------------------------------------
            [config
             "View and modify the package manager's configuration"
             #:usage-help "Shows value for <key>, shows values for all <key>s if"
                          " none is given, or sets a single <key>"
                          " if --set is specified"
             #:once-any
             [#:bool set () "Set <key> to <val>s"]
             #:once-any
             scope-flags ...
             #:handlers
             (lambda (accum . key+vals)
               (call-with-package-scope
                'config
                scope scope-dir installation user #f #f #f #f
                (lambda ()
                  (if set
                      (with-pkg-lock
                       (pkg-config #t key+vals
                                   #:from-command-line? #t))
                      (with-pkg-lock/read-only
                       (pkg-config #f key+vals
                                   #:from-command-line? #t))))))
             (list "key" "val")]
            ;; ----------------------------------------
            [catalog-show
             "Show package information as reported by a catalog"
             #:once-each
             [#:bool all () "Show all packages"]
             [#:bool only-names () "Show only package names"]
             [#:bool modules () "Show implemented modules"]
             catalog-flags ...
             [(#:str vers #f) version ("-v") "Show result for Racket <vers>"]
             #:args pkg-name
             (when (and all (pair? pkg-name))
               ((pkg-error 'catalog-show) "both `--all' and package names provided"))
             (parameterize ([current-pkg-catalogs (and catalog
                                                       (list (catalog->url catalog)))]
                            [current-pkg-error (pkg-error 'catalog-show)]
                            [current-pkg-lookup-version (or version
                                                            (current-pkg-lookup-version))])
               (pkg-catalog-show pkg-name 
                                 #:all? all
                                 #:only-names? only-names
                                 #:modules? modules))]
            ;; ----------------------------------------
            [catalog-copy
             "Copy/merge package name catalogs"
             #:once-each
             [#:bool from-config () "Include currently configured catalogs last"]
             #:once-any
             [#:bool force () "Force replacement of existing file/directory"]
             [#:bool merge () "Merge to existing database"]
             #:once-each
             [#:bool override () "While merging, override existing with new"]
             [#:bool relative () "Make source paths relative when possible"]
             [(#:str vers #f) version ("-v") "Copy information suitable for Racket <vers>"]
             #:args catalog
             (parameterize ([current-pkg-error (pkg-error 'catalog-copy)])
               (when (null? catalog)
                 ((current-pkg-error) "need a destination catalog"))
               (parameterize ([current-pkg-lookup-version (or version
                                                              (current-pkg-lookup-version))])
                 (pkg-catalog-copy (drop-right catalog 1)
                                   (last catalog)
                                   #:from-config? from-config
                                   #:force? force
                                   #:merge? merge
                                   #:override? override
                                   #:relative-sources? relative)))]
            ;; ----------------------------------------
            [catalog-archive
             "Copy catalog plus packages"
             #:once-each
             [#:bool from-config () "Include currently configured catalogs last"]
             [(#:str state-database #f) state () "Read/write <state-database> as state of <dest-dir>"]
             [(#:str vers #f) version ("-v") "Copy information suitable for Racket <vers>"]
             [#:bool relative () "Make source paths relative when possible"]
             [(#:sym mode [fail skip continue] 'fail) pkg-fail ()
              ("Select handling of package-download failure;"
               "<mode>s: fail (the default), skip, continue (but with exit status of 5)")]
             #:args (dest-dir . src-catalog)
             (parameterize ([current-pkg-error (pkg-error 'catalog-archive)]
                            [current-pkg-lookup-version (or version
                                                            (current-pkg-lookup-version))])
                 (define fail-at-end? #f)
                 (pkg-catalog-archive dest-dir
                                      src-catalog
                                      #:from-config? from-config
                                      #:state-catalog state
                                      #:relative-sources? relative
                                      #:package-exn-handler (case pkg-fail
                                                              [(fail) (lambda (name exn) (raise exn))]
                                                              [(skip continue)
                                                               (lambda (name exn)
                                                                 (log-error (~a "archiving failed for package; ~a\n"
                                                                                "  package name: ~a\n"
                                                                                "  original error:\n~a")
                                                                            (if (eq? pkg-fail 'continue)
                                                                                "continuing"
                                                                                "skipping")
                                                                            name
                                                                            (regexp-replace* #rx"(?m:^)"
                                                                                             (exn-message exn)
                                                                                             "   "))
                                                                 (when (eq? pkg-fail 'continue)
                                                                   (set! fail-at-end? #t)))]))
                 (when fail-at-end?
                   (exit 5)))]
            ;; ----------------------------------------
            [archive
             "Create catalog from installed packages"
             (define exclude-list (make-parameter null))
             #:once-each
             [#:bool include-deps () "Include dependencies of specified packages"]
             #:multi
             [(#:str pkg #f) exclude () "Exclude <pkg> from new catalog"
              (exclude-list (cons pkg (exclude-list)))]
             #:once-each
             [#:bool relative () "Make source paths relative when possible"]
             #:args (dest-dir pkg . pkgs)
             (parameterize ([current-pkg-error (pkg-error 'pkgs-archive)])
               (pkg-archive-pkgs dest-dir
                                 (cons pkg pkgs)
                                 #:include-deps? include-deps
                                 #:exclude (exclude-list)
                                 #:relative-sources? relative))]
            ;; ----------------------------------------
            [empty-trash
             "Delete old package installations from the trash directory"
             #:once-any
             scope-flags ...
             #:once-each
             [#:bool list ("-l") "Show trash content without emptying"]
             #:args ()
             (call-with-package-scope
              'empty-trash
              scope scope-dir installation user #f #f #f #f
              (lambda ()
                (pkg-empty-trash #:list? list
                                 #:quiet? #f)))]))]))

  (make-commands
   #:scope-flags
   ([(#:sym scope [installation user] #f) scope ()
     ("Select package <scope>, one of"
      "  installation: for all users of the Racket installation"
      "  user: as user-specific for an installation version/name")]
    [#:bool installation ("-i") "Shorthand for `--scope installation'"]
    [#:bool user ("-u") "Shorthand for `--scope user'"]
    [(#:str dir #f) scope-dir () "Select package scope <dir>"])
   #:dry-run-flags
   ([#:bool dry-run () ("Don't actually change package installation")])
   #:job-flags
   ([#:bool no-setup () ("Don't `raco setup' after changing packages (usually a bad idea)")]
    [(#:num n #f) jobs ("-j") "Setup with <n> parallel jobs"]
    [#:bool batch () ("Disable interactive mode and all prompts")])
   #:trash-flags
   ([#:bool no-trash () ("Delete uninstalled/updated, instead of moving to a trash folder")])
   #:catalog-flags
   ([(#:str catalog #f) catalog () "Use <catalog> instead of configured catalogs"])
   #:install-type-flags
   ([(#:sym type [file dir file-url dir-url git github name] #f) type ("-t") 
     ("Specify type of <pkg-source>, instead of inferred;"
      "valid <types>s are: file, dir, file-url, dir-url, git, github, or name")]
    [(#:str name #f) name ("-n") ("Specify name of package, instead of inferred;"
                                  "makes sense only when a single <pkg-source> is given")]
    [(#:str checksum #f) checksum () ("Checksum of package, either expected or selected;"
                                      "makes sense only when a single <pkg-source> is given")])
   #:install-dep-flags
   ([(#:sym mode [fail force search-ask search-auto] #f) deps ()
     ("Specify the behavior for uninstalled dependencies, with"
      "<mode> as one of"
      "  fail: cancels if dependencies are not installed"
      "  force: continues despite missing dependencies"
      "  search-ask: looks for dependencies in the package catalogs"
      "              and asks for permission to auto-install"
      "  search-auto: like `search-ask', but does not ask for permission")])
   #:install-dep-desc
   ("where the default is `search-ask' in interactive mode, `fail' otherwise")
   #:install-force-flags
   ([#:bool all-platforms () "Follow package dependencies for all platforms"]
    [#:bool force () "Ignore conflicts"]
    [#:bool ignore-checksums () "Ignore checksums"]
    [#:bool strict-doc-conflicts () "Report doc-name conflicts, even for user scope"]
    [#:bool no-cache () "Disable download cache"])
   #:install-clone-flags
   ([(#:sym mode [fail force convert ask] #f) multi-clone ()
     ("Specify treatment of multiple clones of a repository;"
      "<mode>s: convert, ask (interactive default), fail (other default), or force")]
    [(#:sym mode [ff-only try rebase] 'ff-only) pull ()
     ("Specify `git pull' mode for repository clones;"
      "<mode>s: ff-only (the default), try, or rebase")])
   #:update-deps-flags
   ([#:bool update-deps () "For `search-ask' or `search-auto', also update dependencies"]
    [#:bool ignore-implies () "When updating, treat `implies' like other dependencies"])
   #:install-copy-flags
   ([#:bool link () ("Link a directory package source in place (default for a directory)")]
    [#:bool static-link () ("Link in place, promising collections do not change")]
    [#:bool copy () ("Treat directory sources the same as other sources")]
    [(#:str dir #f) clone () ("Clone Git and GitHub package sources to <dir> and link")]
    [#:bool source () ("Strip packages' built elements before installing; implies --copy")]
    [#:bool binary () ("Strip packages' source elements before installing; implies --copy")]
    [#:bool binary-lib () ("Strip source & documentation before installing; implies --copy")])
   #:install-copy-defns
   [(define link-dirs? (not (or copy source binary binary-lib)))
    (define link-type (or (and link 'link) 
                          (and static-link 'static-link)
                          (and (eq? type 'dir) link-dirs? 'link)
                          (and clone 'clone)))
    (define a-type (or link-type type))]
   #:install-copy-checks
   [(when (and type
               link-type
               (not (memq type
                          (case link-type
                            [(clone) '(git github)]
                            [else '(dir)]))))
      ((current-pkg-error) (format "-t/--type value must be ~a with --~a"
                                   (cond
                                    [clone "`git' or `github'"]
                                    [else "`dir'"])
                                   (cond
                                    [link "link"]
                                    [static-link "static-link"]
                                    [clone "clone"]))))])
