#lang racket/base

;; A build farm is normally run via the `installers' target of the
;; Racket repository's top-level makefile. That target, in turn, uses
;; the `distro-build/drive-clients' module.
;;
;; The server machine first prepares packages for installation on
;; clients.  The site configuration's top-level entry is consulted for
;; a `#:pkgs' and/or `#:doc-search' option, which overrides any `PKGS'
;; and/or `DOC_SEARCH' configuration from the makefile.
;;
;; The site configuration file otherwise describes and configures
;; client machines hierarchically, where configuration options
;; propagate down the hierarchy when they are not overridden more
;; locally.
;;
;; Each client is built by running commands via `ssh', where the
;; client's host (and optional port and/or user) indicate the ssh
;; target. Each client machine must be set up with a public-key
;; authenticaion, because a direct `ssh' is expected to work without a
;; password prompt.
;;
;; On the client machine, all work is performed with a git clone at a
;; specified directory. The directory defaults to "build/plt" (Unix,
;; Mac OS X) or "build\\plt" (Windows). If the directory exists
;; already on a client machine (and the machine is not configured for
;; "clean" mode), then the directory is assumed to be a suitable git
;; clone, and it is updated with `git pull'. Otherwise, a git
;; repository is cloned; by default, the server is used as the source
;; git repository (so that the server and client are in sync).
;;
;; If a build fails for a machine, building continues on other
;; machines.  Success for a given machine means that its installer
;; ends up in "build/installers" (and failure for a machine means no
;; installer) as recorded in the "table.rktd" file.
;; 
;; Machine Requirements
;; --------------------
;;
;; Each Unix or Mac OS X client needs the following available:
;;
;;   * ssh server with public-key authentication
;;   * git
;;   * gcc, make, etc.
;;
;; Each Windows client needs the following:
;;
;;   * git
;;   * Microsoft Visual Studio 9.0 (2008), installed in the
;;     default folder:
;;      C:\Program Files\Microsoft Visual Studio 9.0       (32-bit host)
;;      C:\Program Files (x86)\Microsoft Visual Studio 9.0 (64-bit host)
;;   * Nullsoft Scriptable Install System (NSIS), installed in the
;;     default folder:
;;      C:\Program Files\NSIS\makensis.exe
;;      or  C:\Program Files (x86)\NSIS\makensis.exe
;;     or instaled so that `makensis' in in yur PATH.
;;
;; Site Configuration
;; -------------------
;;
;; A site configuration module is normally wriiten in the
;; `distro-build/config' language. The configuration describes
;; individual machines, and groups them with `parallel' or
;; `sequential' to indicate whether the machine's builds should run
;; sequentially or in parallel.  Options specified at `parallel' or
;; `sequential' are propagated to each machine in the group.
;;
;; For example, a configuration module might look like this:
;;
;;     #lang distro-build/config
;;    
;;     (sequential
;;      #:pkgs '("drracket")
;;      #:server "192.168.56.1"
;;      (machine
;;       #:desc "Linux (32-bit, Precise Pangolin)"
;;       #:name "Ubuntu 32"
;;       #:vbox "Ubuntu 12.04"
;;       #:host "192.168.56.102")
;;      (machine
;;       #:desc "Windows (64-bit)"
;;       #:name "Windows 64"
;;       #:vbox "Windows 7"
;;       #:host "192.168.56.103"
;;       #:port 2022
;;       #:dir "c:\\Users\\mflatt\\build\\plt"
;;       #:platform 'windows
;;       #:bits 64))
;;
;;
;; Site-configuration keywords (where <string*> means no spaces, etc.):
;;
;;   #:host <string*> --- defaults to "localhost"
;;   #:port <integer> --- ssh port for the client; defaults to 22
;;   #:user <string*> --- ssh user for the client; defaults to current user
;;   #:dir <string> --- defaults to "build/plt" or "build\\plt"
;;   #:server <string*> --- the address of the server as accessed by the
;;                          client; defaults to the `server' command-line
;;                          argument
;;   #:repo <string> --- the git repository for Racket; defaults to
;;                       "http://<server>:9440/.git"
;;   #:pkgs '(<string*> ...) --- packages to install; defaults to
;;                              `PKGS' in the makefile (or, particularly,
;;                              the `pkgs' command-line argument to
;;                              `distro-build/drive-clients')
;;   #:dist-base-url <string> --- a URL that is used to construct
;;                                a default for #:doc-search and
;;                                #:dist-catalogs, where the
;;                                constructed values are consistent
;;                                with converting a build server's
;;                                content into a download site; since
;;                                URLs are constructed via relative
;;                                paths, this URL normally should end
;;                                with a slash
;;   #:doc-search <string> --- URL to install as the configuration
;;                             for remote documentation searches in
;;                             generated installers; "" is replaced
;;                             with the PLT default; defaults to
;;                             #:dist-base-url (if present) extended
;;                             with "doc/search.html", or the
;;                             `DOC_SEARCH' makefile variable (or the
;;                             `doc-search' argument)
;;   #:dist-name <string> --- the distribution name; defaults to the
;;                            `DIST_NAME' makefile variable (or the
;;                            `dist-name' command-line argument)
;;   #:dist-base <string*> --- the distribution's installater name prefix;
;;                             defaults to the `DIST_BASE' makefile variable
;;                             (or the `dist-base' command-line argument)
;;   #:dist-dir <string*> --- the distribution's installation directory;
;;                            defaults to the `DIST_DIR' makefile variable
;;                            (or the `dist-dir' command-line argument)
;;   #:dist-suffix <string*> --- a suffix for the installer's name, usually
;;                               used for an OS variant; defaults to the
;;                               `DIST_SUFFIX' makefile variable (or the
;;                               `dist-suffix' command-line argument)
;;   #:dist-catalogs '(<string> ...) --- catalog URLs to install as the
;;                                       initial catalog configuration
;;                                       in generated installed, where
;;                                       "" is replaced with the PLT
;;                                       default catalogs; defaults to
;;                                       #:dist-base-url (if present)
;;                                       extended with "catalogs" in a
;;                                       list followed by ""
;;   #:max-vm <real> --- max number of VMs allowed to run with this
;;                       machine, counting the machine; defaults to 1
;;   #:vbox <string> --- Virtual Box machine name; if provided the
;;                       virtual machine is started and stopped as needed
;;   #:platform <symbol> --- 'windows or 'unix, defaults to 'unix
;;   #:configure '(<string> ...) --- arguments to `configure'
;;   #:bits <integer> --- 32 or 64, affects Visual Studio path
;;   #:vc <string*> --- "x86" or "x64" to select the Visual C build mode;
;;                     default depends on bits
;;   #:j <integer> --- parallelism for `make' on Unix and Mac OS X;
;;                     defaults to 1
;;   #:timeout <number> --- numbers of seconds to wait before declaring
;;                          failure; defaults to 30 minutes
;;   #:clean? <boolean> --- if true, then the build process on the client
;;                          machine starts by removing <dir>; set this
;;                          to #f for a shared repo checkout; the default
;;                          is determined by the `CLEAN_MODE' makefile
;;                          variable (or `--clean' command-line flag)
;;   #:pull? <boolean> --- if true, then the build process on the client
;;                         machine starts by a `git pull' in <dir>; set
;;                         to #f, for example, for a repo checkout that is
;;                         shared with server; the default is #t
;;
;; Machine-only keywords:
;;
;;   #:name <string> --- defaults to host; this string is recorded as
;;                       a description of the installer (for use in a
;;                       generated table of installer links, for example)
;;
;;
;; More precisely, the `distro-build/config' language is like
;; `racket/base' except that the module body must have exactly one
;; expression (plus any number of definitions, etc.) that produces a
;; site-configuration value. The value is exported as `site-config'
;; from the module. Any module can act as a site-configuration module
;; a long as it exports `site-config' as a site-configuration value.
;;
;; The `distro-build/config' language also adds the following functions
;; to `racket/base':
;;
;;  (machine <opt-kw> <opt-val> ... ...) -> site-config?
;;    Produces a site configuration based on the given keyword-based
;;    options. The support keyword arguments are described above.
;;
;;  (sequential <opt-kw> <opt-val> ... ... config ...)
;;    -> site-config?
;;     config : site-config?
;;    Produces a site configuration that runs each `config'
;;    sequentially. The support keyword arguments are described above.
;;
;;  (parallel <opt-kw> <opt-val> ... ... config ...)
;;    -> site-config?
;;     config : site-config?
;;    Produces a site configuration that runs each `config' in
;;    parallel. The support keyword arguments are described above.
;;
;;  (site-config? v) -> boolean?
;;  (site-config-tag config) -> (or/c 'machine 'sequential 'parallel)
;;     config : site-config?
;;  (site-config-options config) -> (hash/c keyword? any/c)
;;     config : site-config?
;;  (site-config-content config) -> (listof site-config?)
;;     config : site-config?
;;   Site configuation inspection
;;
;;  (current-mode) -> string?
;;  (current-mode s) -> void?
;;     s : string?
;;   A parameter whose value is the user's requested mode for this
;;   configuration, normally as provided via the makefile's
;;   `CONFIG_MODE' variable. The default mode is "default". The
;;   interpretation of modes is completely up to the
;;   site configuration file.

;; ----------------------------------------

(require racket/format
         (for-syntax syntax/kerncase
                     racket/base))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         sequential
         parallel
         machine
         site-config?
         site-config-tag
         site-config-options
         site-config-content
         current-mode
         extract-options)

(module reader syntax/module-reader
  distro-build/config)

(struct site-config (tag options content))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (site-begin #f form ...)))

(define-syntax (site-begin stx)
  (syntax-case stx ()
    [(_ #t) #'(begin)]
    [(_ #f)
     (raise-syntax-error 'site
                         "did not find an expression for the site configuration")]
    [(_ found? next . rest) 
     (let ([expanded (local-expand #'next 'module (kernel-form-identifier-list))])
       (syntax-case expanded (begin)
         [(begin next1 ...)
          #`(site-begin found? next1 ... . rest)]
         [(id . _)
          (and (identifier? #'id)
               (ormap (lambda (kw) (free-identifier=? #'id kw))
                      (syntax->list #'(require
                                       provide
                                       define-values
                                       define-syntaxes
                                       begin-for-syntax
                                       module
                                       module*
                                       #%require
                                       #%provide))))
          #`(begin #,expanded (site-begin found? . rest))]
         [_else
          (if (syntax-e #'found?)
              (raise-syntax-error 'site
                                  "found second top-level expression"
                                  #'next)
              #`(begin
                 (provide site-config)
                 (define site-config (let ([v #,expanded])
                                       (unless (site-config? v)
                                         (error 'site
                                                (~a "expression did not produce a site configuration\n"
                                                    "  result: ~e\n"
                                                    "  expression: ~.s")
                                                v
                                                'next))
                                       v))
                 (site-begin
                  #t
                  . rest)))]))]))

(define sequential
  (make-keyword-procedure
   (lambda (kws kw-vals . subs)
     (constructor kws kw-vals subs
                  check-group-keyword 'sequential))))
(define parallel
  (make-keyword-procedure
   (lambda (kws kw-vals . subs)
     (constructor kws kw-vals subs
                  check-group-keyword 'sequential))))
(define machine
  (make-keyword-procedure
   (lambda (kws kw-vals)
     (constructor kws kw-vals null
                  check-machine-keyword 'machine))))

(define (constructor kws kw-vals subs check tag)
  (site-config
   tag
   (for/hash ([kw (in-list kws)]
              [val (in-list kw-vals)])
     (define r (check kw val))
     (when (eq? r 'bad-keyword)
       (error tag
              (~a "unrecognized keyword for option\n"
                  "  keyword: ~s")
              kw))
     (unless (check kw val)
       (error tag
              (~a "bad value for keyword\n"
                  "  keyword: ~s\n"
                  "  value: ~e")
              kw
              val))
     (values kw val))
   (for/list ([sub subs])
     (unless (site-config? sub)
       (raise-argument-error tag "site-config?" sub))
     sub)))

(define (check-group-keyword kw val)
  (case kw
    [(#:pkgs) (and (list? val) (andmap simple-string? val))]
    [(#:doc-search) (string? val)]
    [(#:dist-name) (string? val)]
    [(#:dist-base) (simple-string? val)]
    [(#:dist-dir) (simple-string? val)]
    [(#:dist-suffix) (simple-string? val)]
    [(#:dist-catalogs) (and (list? val) (andmap string? val))]
    [(#:dist-base-url) (string? val)]
    [(#:max-vm) (real? val)]
    [(#:server) (simple-string? val)]
    [(#:host) (simple-string? val)]
    [(#:user) (simple-string? val)]
    [(#:port) (and (exact-integer? val) (<= 1 val 65535))]
    [(#:dir) (string? val)]
    [(#:vbox) (string? val)]
    [(#:platform) (memq val '(unix windows))]
    [(#:configure) (and (list? val) (andmap string? val))]
    [(#:bits) (or (equal? val 32) (equal? val 64))]
    [(#:vc) (or (equal? val "x86") (equal? val "x64"))]
    [(#:timeout) (real? val)]
    [(#:j) (exact-positive-integer? val)]
    [(#:repo) (string? val)]
    [(#:clean?) (boolean? val)]
    [(#:pull?) (boolean? val)]
    [else 'bad-keyword]))

(define (check-machine-keyword kw val)
  (case kw
    [(#:name) (string? val)]
    [else (check-group-keyword kw val)]))

(define (simple-string? s)
  (and (string? s)
       ;; No spaces, quotes, or other things that could
       ;; break a command-line, path, or URL construction:
       (regexp-match #rx"^[-a-zA-A0-9.]*$" s)))

(define current-mode (make-parameter "default"))

(define (extract-options config-file config-mode)
  (or
   (and (file-exists? config-file)
        (parameterize ([current-mode config-mode])
          (site-config-options 
           (dynamic-require (path->complete-path config-file) 'site-config))))
   (hash)))

