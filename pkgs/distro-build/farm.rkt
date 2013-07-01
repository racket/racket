#lang racket/base

;; A build farm is normally run via the `farm' target of the Racket
;; repository's top-level makefile. That target, in turn, uses the
;; `distro-build/drive-clients' module.
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
;; Farm Configuration
;; -------------------
;;
;; A farm configuration module is normally wriiten in the
;; `distro-build/farm' language. The configuration describes
;; individual machines, and groups them with `parallel' or
;; `sequential' to indicate whether the machine's builds should run
;; sequentially or in parallel.  Options specified at `parallel' or
;; `sequential' are propagated to eachmachine in the group.
;;
;; For example, a configuration module might look like this:
;;
;;     #lang distro-build/farm
;;    
;;     (sequential
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
;; Farm-configuration keywords (where <string*> means no spaces, etc.):
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
;;                              `PKGS' in the makfile (or, more genereally,
;;                              the `pkgs' command-line argument to
;;                              `distro-build/drive-clients')
;;   #:dist-name <string> --- the distribution name; defaults to the
;;                            `DIST_NAME' makefile variable or `dist-name'
;;                            command-line argument
;;   #:dist-base <string*> --- the distribution's installater name prefix;
;;                             defaults to the `DIST_BASE' makefile variable
;;                             or the `dist-base' command-line argument
;;   #:dist-dir <string*> --- the distribution's installation directory;
;;                            defaults to the `DIST_DIR' makefile variable
;;                            or the `dist-dir' command-line argument
;;   #:dist-suffix <string*> --- a suffix for the installer's name, usually
;;                               used for an OS variant; defaults to the
;;                               `DIST_SUFFIX' makefile variable or the
;;                               `dist-suffix' command-line argument
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
;;                          variable or `--clean' command-line flag
;;   #:pull? <boolean> --- if true, then the build process on the client
;;                         machine starts by a `git pull' in <dir>; set
;;                         to #f, for example, for a repo checkout that is
;;                         shared with server; the default is #t
;;
;; Machine-only keywords:
;;   #:name <string> --- defaults to host; this string is recorded as
;;                       a description of the installer (for use in a
;;                       generated table of installer links, for example)
;;
;;
;; More precisely, the `distro-build/farm' language is like
;; `racket/base' except that the module body must have exactly one
;; expression (plus any number of definitions, etc.) that produces a
;; farm-configuration value. The value is exported as `farm-config'
;; from the module. Any module can act as a farm-configuration module
;; a long as it exports `farm-config' as a farm-configuration value.
;;
;; The `distro-build/farm' language also adds the following functions
;; to `racket/base':
;;
;;  (machine <opt-kw> <opt-val> ... ...) -> farm-config?
;;    Produces a farm configuration based on the given keyword-based
;;    options. The support keyword arguments are described above.
;;
;;  (sequential <opt-kw> <opt-val> ... ... config ...)
;;    -> farm-config?
;;     config : farm-config?
;;    Produces a farm configuration that runs each `config'
;;    sequentially. The support keyword arguments are described above.
;;
;;  (parallel <opt-kw> <opt-val> ... ... config ...)
;;    -> farm-config?
;;     config : farm-config?
;;    Produces a farm configuration that runs each `config' in
;;    parallel. The support keyword arguments are described above.
;;
;;  (farm-config? v) -> boolean?
;;  (farm-config-tag config) -> (or/c 'machine 'sequential 'parallel)
;;     config : farm-config?
;;  (farm-config-options config) -> (hash/c keyword? any/c)
;;     config : farm-config?
;;  (farm-config-content config) -> (listof farm-config?)
;;     config : farm-config?
;;   Farm configuation inspection
;;
;;  (current-mode) -> string?
;;  (current-mode s) -> void?
;;     s : string?
;;   A parameter whose value is the user's requested mode for this
;;   configuration. The default mode is "default". The interpretation
;;   of modes is completely up to the farm-configuration file.

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
         farm-config?
         farm-config-tag
         farm-config-options
         farm-config-content
         current-mode)

(module reader syntax/module-reader
  distro-build/farm)

(struct farm-config (tag options content))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (farm-begin #f form ...)))

(define-syntax (farm-begin stx)
  (syntax-case stx ()
    [(_ #t) #'(begin)]
    [(_ #f)
     (raise-syntax-error 'farm
                         "did not find an expression for the farm configuration")]
    [(_ found? next . rest) 
     (let ([expanded (local-expand #'next 'module (kernel-form-identifier-list))])
       (syntax-case expanded (begin)
         [(begin next1 ...)
          #`(farm-begin found? next1 ... . rest)]
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
          #`(begin #,expanded (farm-begin found? . rest))]
         [_else
          (if (syntax-e #'found?)
              (raise-syntax-error 'farm
                                  "found second top-level expression"
                                  #'next)
              #`(begin
                 (provide farm-config)
                 (define farm-config (let ([v #,expanded])
                                       (unless (farm-config? v)
                                         (error 'farm
                                                (~a "expression did not produce a farm configuration\n"
                                                    "  result: ~e\n"
                                                    "  expression: ~.s")
                                                v
                                                'next))
                                       v))
                 (farm-begin
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
  (farm-config
   tag
   (for/hash ([kw (in-list kws)]
              [val (in-list kw-vals)])
     (unless (check kw val)
       (error tag
              (~a "bad value for keyword\n"
                  "  keyword: ~s"
                  "  value: ~e")
              kw
              val))
     (values kw val))
   (for/list ([sub subs])
     (unless (farm-config? sub)
       (raise-argument-error tag "farm-config?" sub))
     sub)))

(define (check-group-keyword kw val)
  (case kw
    [(#:pkgs) (and (list? val) (andmap simple-string? val))]
    [(#:dist-name) (string? val)]
    [(#:dist-base) (simple-string? val)]
    [(#:dist-dir) (simple-string? val)]
    [(#:dist-suffix) (simple-string? val)]
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
    [else #f]))

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
