#lang racket/base

;; Each client is built by running commands via `ssh', where the
;; client's host (and optional port and/or user) indicate the ssh
;; target. Each client machine must be set up with a public-key
;; authenticaion, because a direct `ssh' is expected to work without a
;; password prompt.
;;
;; On the client machine, all work is performed with a git clone at a
;; specified directory that defaults to "build/plt" (Unix, Mac OS X)
;; or "build\\plt" (Windows).
;;
;; If a build fails for a machine, building continues on other
;; machines.  Success for a given machine means that its installer
;; ends up in "build/installers" (and failure for a machine means no
;; installer).
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
;; A farm configuration file is `read' to obtain a configuration. The
;; file must have a single S-expression that matches the <config>
;; grammar:
;; 
;;    <config> = (machine <keyword> <val> ... ...)
;;             | (<group-kind> <keyword> <val> ... ... <config> ...)
;; 
;;    <group-kind> = parallel | sequential
;;
;; Normally, a configuration file start with "(<group-kind> ...)", since
;; the configuration otherwise specifies only one client machine.
;;
;; A `<keyword> <val> ... ...' sequence specifies options as
;; keyword--value pairs. The available options are listed below. The
;; options of a group are propagated to all machines in the group,
;; except at overridden at a machine or nested group.
;;
;; A <group-kind> specifies whether the machines within a group are
;; run sequentially or in parallel. Note that the default`#:max-vm'
;; setting is 1, so a parallel configuration of virtual machines will
;; fail (for some machines) unless `#:max-vm' is increased.
;;
;; Machine/group keywords (where <string*> means no spaces, etc.):
;;
;;   #:pkgs (<string*> ...) --- packages to install; defaults to
;;                              the `pkgs' command-line argument
;;   #:server <string*> --- the address of the server from the client;
;;                          defaults to `server' command-line argument
;;   #:dist-name <string> --- the distribution name; defaults to the
;;                            `dist-name' command-line argument
;;   #:dist-dir <string> --- the distribution's installation directory;
;;                           defaults to `dist-dir' command-line argument
;;   #:max-vm <real> --- max number of VMs allowed to run with this
;;                       machine, counting the machine; defaults to 1
;;   #:port <integer> --- ssh port for the client; defaults to 22
;;   #:user <string*> --- ssh user for the client; defaults to current user
;;   #:dir <string> --- defaults to "build/plt" or "build\\plt"
;;   #:vbox <string> --- Virtual Box machine name; if provided the
;;                       virtual machine is started and stopped as needed
;;   #:platform <symbol> --- 'windows or 'unix, defaults to 'unix
;;   #:bits <integer> --- 32 or 64, affects Visual Studio path
;;   #:vc <string*> --- "x86" or "x64" to select the Visual C build mode;
;;                     default depends on bits
;;   #:j <integer> --- parallelism for `make' on Unix and Mac OS X;
;;                     defaults to 1
;;   #:timeout <number> --- numbers of seconds to wait before declaring
;;                          failure; defaults to 30 minutes
;;   #:repo <string> --- the git repository for Racket; defaults to
;;                       "http://<server>:9440/.git"
;;
;; Machine-only keywords:
;;   #:name <string> --- defaults to host
;;   #:host <string*> --- defaults to "localhost"

;; ----------------------------------------

(require racket/cmdline
         racket/system
         racket/port
         racket/format
         racket/file
         racket/string)

;; ----------------------------------------

(define release? #f)

(define-values (config-file default-server default-pkgs default-dist-name default-dist-dir)
  (command-line
   #:once-each
   [("--release") "Create release-mode installers"
    (set! release? #t)]
   #:args (config-file server pkgs dist-name dist-dir)
   (values config-file server pkgs dist-name dist-dir)))

(define config (call-with-input-file* config-file read))

;; ----------------------------------------

(define (simple-string? s)
  (and (string? s)
       ;; No spaces, quotes, or other things that could
       ;; break a command-line, path, or URL construction:
       (regexp-match #rx"^[-a-zA-A0-9.]*$" s)))

(define (check-group-keyword kw val)
  (case kw
    [(#:pkgs) (and (list? val) (andmap simple-string? val))]
    [(#:dist-name) (string? val)]
    [(#:dist-dir) (simple-string? val)]
    [(#:max-vm) (real? val)]
    [(#:server) (simple-string? val)]
    [(#:host) (simple-string? val)]
    [(#:user) (simple-string? val)]
    [(#:port) (and (exact-integer? val) (<= 1 val 65535))]
    [(#:dir) (string? val)]
    [(#:vbox) (string? val)]
    [(#:platform) (memq val '(unix windows))]
    [(#:bits) (or (equal? val 32) (equal? val 64))]
    [(#:vc) (or (equal? val "x86") (equal? val "x64"))]
    [(#:timeout) (real? val)]
    [(#:j) (exact-positive-integer? val)]
    [(#:repo) (string? val)]
    [else #f]))

(define (check-machine-keyword kw val)
  (case kw
    [(#:name) (string? val)]
    [else (check-group-keyword kw val)]))

(define (check-config config)
  (define (bad-format msg . rest)
    (raise-user-error 'drive-clients
                      "~a"
                      (apply ~a "bad configuration"
                             "\n " msg
                             (if config-file
                                 (~a "\n  config file: "
                                     config-file)
                                 "")
                             rest)))
  (unless (list? config)
    (bad-format (if config-file
                    "does not `read' as a list"
                    "not a list")))
  (let loop ([config config])
    (unless (list? config)
      (bad-format "not a list"
                  (format "\n  given: ~e" config)))
    (cond
     [(and (pair? config)
           (or (eq? 'parallel (car config))
               (eq? 'sequential (car config))))
      (let gloop ([group (cdr config)])
        (cond
         [(keyword? (car group))
          (unless (pair? (cdr group))
            (bad-format "missing value after group keyword"
                        (format "\n  keyword: ~e" (car group))))
          (unless (check-group-keyword (car group) (cadr group))
            (bad-format "bad value for keyword in group"
                        (format "\n  keyword: ~e\n  value: ~e"
                                (car group)
                                (cadr group))))
          (gloop (cddr group))]
         [else (for-each loop group)]))]
     [(and (pair? config)
           (eq? 'machine (car config)))
      (let loop ([client (cdr config)])
        (cond
         [(null? client) (void)]
         [(keyword? (car client))
          (unless (pair? (cdr client))
            (bad-format "machine spec missing value after keyword"
                        (format "\n  keyword: ~e" (car client))))
          (unless (check-machine-keyword (car client) (cadr client))
            (bad-format "bad value for keyword in machine spec"
                        (format "\n  keyword: ~e\n  value: ~e"
                                (car client)
                                (cadr client))))
          (loop (cddr client))]
         [else
          (bad-format "bad machine spec; expected a keyword"
                      (format "\n  found: ~e" (car client)))]))]
     [else 
      (bad-format "bad format (does not start with 'machine, 'parallel, or 'sequential)"
                  (format "\n  found: ~e" config))])))

(check-config config)

;; ----------------------------------------

(define (merge-options opts c)
  (let loop ([c (cdr c)] [opts opts])
    (cond
     [(and (pair? c)
           (keyword? (car c)))
      (loop (cddr c)
            (hash-set opts (car c) (cadr c)))]
     [else opts])))

(define (get-opt opts kw)
  (hash-ref opts kw #f))

(define (get-content c)
  (let loop ([c (cdr c)])
    (if (and (pair? c)
             (keyword? (car c)))
        (loop (cddr c))
        c)))

(define (client-name opts)
  (or (get-opt opts '#:name)
      (get-opt opts '#:host)
      "localhost"))

;; ----------------------------------------
;; Managing VirtualBox machines

(define VBoxManage (find-executable-path "VBoxManage"))
(define use-headless? #t)

(define (system*/show exe . args)
  (displayln (apply ~a #:separator " " 
                    (map (lambda (p) (if (path? p) (path->string p) p)) 
                         (cons exe args))))
  (apply system* exe args))

(define (system*/string . args)
  (define s (open-output-string))
  (parameterize ([current-output-port s])
    (apply system* args))
  (get-output-string s))

(define (vbox-state vbox)
  (define s (system*/string VBoxManage "showvminfo" vbox))
  (define m (regexp-match #rx"(?m:^State:[ ]*([a-z]+(?: [a-z]+)*))" s))
  (define state (and m (string->symbol (cadr m))))
  (case state
    [(|powered off| aborted) 'off]
    [(running saved paused) state]
    [(restoring) (vbox-state vbox)]
    [else 
     (eprintf "~a\n" s)
     (error 'vbox-state "could not get virtual machine status: ~s" vbox)]))

(define (vbox-control vbox what)
  (system* VBoxManage "controlvm" vbox what))

(define (vbox-start vbox)
  (apply system* VBoxManage "startvm" vbox 
         (if use-headless?
             '("--type" "headless")
             null))
  ;; wait for the machine to get going:
  (let loop ([n 0])
    (unless (eq? 'running (vbox-state vbox))
      (unless (= n 20)
        (sleep 0.5)
        (loop (add1 n))))))

(define call-with-vbox-lock
  (let ([s (make-semaphore 1)]
        [lock-cust (current-custodian)])
    (lambda (thunk)
      (define t (current-thread))
      (define ready (make-semaphore))
      (define done (make-semaphore))
      (parameterize ([current-custodian lock-cust])
        (thread (lambda () 
                  (semaphore-wait s)
                  (semaphore-post ready)
                  (sync t done)
                  (semaphore-post s))))
      (sync ready)
      (thunk)
      (semaphore-post done))))

(define (start-client c max-vm)
  (define vbox (get-opt c '#:vbox))
  (define (check-count)
    (define s (system*/string VBoxManage "list" "runningvms"))
    (unless ((length (string-split s "\n")) . < . max-vm)
      (error 'start-client "too many virtual machines running (>= ~a) to start: ~s"
             max-vm
             (client-name c))))
  (when vbox
    (printf "Starting VirtualBox machine ~s\n" vbox)
    (case (vbox-state vbox)
      [(running) (void)]
      [(paused) (vbox-control vbox "resume")]
      [(off saved) (call-with-vbox-lock
                    (lambda ()
                      (check-count)
                      (vbox-start vbox)))])
    (unless (eq? (vbox-state vbox) 'running)
      (error 'start-client "could not get virtual machine started: ~s" (client-name c))))
  ;; pause a little to let the VM get networkign ready, etc.
  (sleep 3))

(define (stop-client c)
  (define vbox (get-opt c '#:vbox))
  (when vbox
    (printf "Stopping VirtualBox machine ~s\n" vbox)
    (vbox-control vbox "savestate")
    (unless (eq? (vbox-state vbox) 'saved)
      (error 'start-client "virtual machine isn't in the expected saved state: ~s" c))))

;; ----------------------------------------

(define scp (find-executable-path "scp"))
(define ssh (find-executable-path "ssh"))

(define (ssh-script host port user . cmds)
  (for/and ([cmd (in-list cmds)])
    (apply system*/show ssh 
           "-p" (~a port)
           (if user 
               (~a user "@" host)
               host)
           cmd)))

(define (q s)
  (~a "\"" s "\""))

(define (client-args server pkgs dist-name dist-dir)
  (~a " SERVER=" server
      " PKGS=" (q pkgs)
      " DIST_NAME=" (q dist-name)
      " DIST_DIR=" dist-dir
      " RELEASE_MODE=" (if release? "--release" (q ""))))

(define (unix-build c host port user server repo
                    pkgs dist-name dist-dir)
  (define dir (or (get-opt c '#:dir)
                  "build/plt"))
  (define (sh . args) 
    (list "/bin/sh" "-c" (~a "'" (apply ~a args) "'")))
  (define j (or (get-opt c '#:j) 1))
  (ssh-script
   host port user
   (sh "if [ ! -d " (q dir) " ] ; then"
       " git clone " (q repo) " " (q dir) " ; "
       "fi")
   (sh "cd " (q dir) " ; "
       "git pull")
   (sh "cd " (q dir) " ; "
       "make -j " j " client"
       (client-args server pkgs dist-name dist-dir))))

(define (windows-build c host port user server repo
                       pkgs dist-name dist-dir)
  (define dir (or (get-opt c '#:dir)
                  "build\\plt"))
  (define bits (or (get-opt c '#:bits) 64))
  (define vc (or (get-opt c '#:vc)
                 (if (= bits 32)
                     "x86"
                     "x64")))
  (define (cmd . args) 
    (list "cmd" "/c" (apply ~a args)))
  (ssh-script
   host port user
   (cmd "IF NOT EXIST " (q dir) " git clone " (q repo) " " (q dir))
   (cmd "cd " (q dir)
        " && git pull")
   (cmd "cd " (q dir)
        " && \"c:\\Program Files" (if (= bits 64) " (x86)" "") "\\Microsoft Visual Studio 9.0\\vc\\vcvarsall.bat\""
        " " (if (= bits 64) "x64" "x86")
        " && nmake win32-client" (client-args server pkgs dist-name dist-dir))))

(define (client-build c)
  (define host (or (get-opt c '#:host)
                   "localhost"))
  (define port (or (get-opt c '#:port)
                   22))
  (define user (get-opt c '#:user))
  (define server (or (get-opt c '#:server)
                     default-server))
  (define pkgs (or (get-opt c '#:pkgs)
                   default-pkgs))
  (define dist-name (or (get-opt c '#:dist-name)
                        default-dist-name))
  (define dist-dir (or (get-opt c '#:dist-dir)
                       default-dist-dir))
  (define repo (or (get-opt c '#:repo)
                   (~a "http://" server ":9440/.git")))
  ((case (or (get-opt c '#:platform) 'unix)
     [(unix) unix-build]
     [else windows-build])
   c host port user server repo
   pkgs dist-name dist-dir))

;; ----------------------------------------

(define (limit-and-report-failure c timeout-factor thunk)
  (define cust (make-custodian))
  (define timeout (or (get-opt c '#:timeout)
                      (* 30 60)))
  (define orig-thread (current-thread))
  (parameterize ([current-custodian cust])
    (thread (lambda ()
              (sleep (* timeout-factor timeout))
              ;; try nice interrupt, first:
              (break-thread orig-thread)
              (sleep 1)
              ;; force quit:
              (custodian-shutdown-all cust)))
    (with-handlers ([exn? (lambda (exn)
                            (log-error "~a failed..." (client-name c))
                            (log-error (exn-message exn)))])
      (thunk)))
  (custodian-shutdown-all cust))

(define (client-thread c sequential? thunk)
  (define log-dir (build-path "build" "drive"))
  (define log-file (build-path log-dir (client-name c)))
  (make-directory* log-dir)
  (printf "Logging build: ~a\n" log-file)
  (define (go)
    (define p (open-output-file log-file
                                #:exists 'truncate/replace))
    (file-stream-buffer-mode p 'line)
    (parameterize ([current-output-port p]
                   [current-error-port p])
      (thunk)))
  (cond
   [sequential? (go) (thread void)]
   [else (thread go)]))

;; ----------------------------------------

(void
 (let loop ([config config]
            [mode 'sequential]
            [opts (hasheq)])
   (case (car config)
     [(parallel sequential)
      (define new-opts (merge-options opts config))
      (define ts
        (map (lambda (c) (loop c
                               (car config)
                               new-opts))
             (get-content config)))
      (define (wait)
        (for ([t (in-list ts)])
          (sync t)))
      (cond
       [(eq? mode 'sequential) (wait) (thread void)]
       [else (thread wait)])]
     [else 
      (define c (merge-options opts config))
      (client-thread
       c
       (eq? mode 'sequential)
       (lambda ()
         (limit-and-report-failure
          c 2
          (lambda ()
            ;; start client, if a VM:
            (start-client c (or (get-opt c '#:max-vm) 1))
            ;; catch failure in build step proper, so we
            ;; can more likely stop the client:
            (limit-and-report-failure
             c 1
             (lambda () (client-build c)))
            ;; stop client, if a VM:
            (stop-client c)))))])))
