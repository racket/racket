#lang racket/base
(require racket/cmdline
         racket/system
         racket/port
         racket/format
         racket/file
         racket/string
         racket/path
         (only-in distro-build/config
                  current-mode
                  site-config?
                  site-config-tag site-config-options site-config-content
                  current-stamp)
         distro-build/url-options
         distro-build/display-time
         distro-build/readme
         remote-shell/vbox
         "email.rkt")

;; See "config.rkt" for an overview.

(module test racket/base)

;; ----------------------------------------

(define default-release? #f)
(define default-source? #f)
(define default-versionless? #f)
(define default-clean? #f)
(define dry-run #f)

(define snapshot-install-name "snapshot")

(define-values (config-file config-mode
                            default-server default-server-port default-server-hosts
                            default-pkgs default-doc-search
                            default-dist-name default-dist-base default-dist-dir)
  (command-line
   #:once-each
   [("--release") "Create release-mode installers"
    (set! default-release? #t)]
   [("--source") "Create source installers"
    (set! default-source? #t)]
   [("--versionless") "Avoid version number in names and paths"
    (set! default-versionless? #t)]
   [("--clean") "Erase client directories before building"
    (set! default-clean? #t)]
   [("--dry-run") mode
    ("Don't actually use the clients;"
     " <mode> can be `ok', `fail', `error', `stuck', or `frozen'")
    (unless (member mode '("ok" "fail" "error" "stuck" "frozen"))
      (raise-user-error 'drive-clients "bad dry-run mode: ~a" mode))
    (set! dry-run (string->symbol mode))]
   #:args (config-file config-mode 
                       server server-port server-hosts pkgs doc-search
                       dist-name dist-base dist-dir)
   (values config-file config-mode
           server server-port server-hosts pkgs doc-search
           dist-name dist-base dist-dir)))

(define config (parameterize ([current-mode config-mode])
                 (dynamic-require (path->complete-path config-file) 'site-config)))

(unless (site-config? config)
  (error 'drive-clients 
         "configuration module did not provide a site-configuration value: ~e"
         config))

;; ----------------------------------------

(define (merge-options opts c)
  (for/fold ([opts opts]) ([(k v) (in-hash (site-config-options c))])
    (if (eq? k '#:custom)
        (hash-set opts
                  '#:custom
                  (let ([prev (hash-ref opts '#:custom (hash))])
                    (for/fold ([prev prev]) ([(k2 v2) (in-hash v)])
                      (hash-set prev k2 v2))))
        (hash-set opts k v))))

(define (get-opt opts kw [default #f] #:localhost [localhost-default default])
  (hash-ref opts kw (lambda ()
                      (cond
                       [(equal? default localhost-default) default]
                       [(and (equal? "localhost" (get-opt opts '#:host "localhost"))
                             (equal? #f (get-opt opts '#:user #f))
                             (equal? #f (get-opt opts '#:dir #f)))
                        localhost-default]
                       [else default]))))

(define (get-content c)
  (site-config-content c))

(define (client-name opts)
  (or (get-opt opts '#:name)
      (get-opt opts '#:host)
      "localhost"))

(define (get-path-opt opt key default #:localhost [localhost-default default])
  (define d (get-opt opt key default #:localhost localhost-default))
  (if (path? d)
      (path->string d)
      d))

(define (add-defaults c . l)
  (let loop ([c c] [l l])
    (cond
     [(null? l) c]
     [else (loop (hash-set c (car l) 
                           (hash-ref c (car l) (lambda () (cadr l))))
                 (cddr l))])))

;; ----------------------------------------
;; Managing VirtualBox machines

(define (start-client c max-vm)
  (define vbox (get-opt c '#:vbox))
  (when vbox
    (start-vbox-vm vbox
                   #:max-vms max-vm
                   #:dry-run? dry-run)))

(define (stop-client c)
  (define vbox (get-opt c '#:vbox))
  (when vbox
    (stop-vbox-vm vbox)))

;; ----------------------------------------

(define scp (find-executable-path "scp"))
(define ssh (find-executable-path "ssh"))

(define (system*/show exe . args)
  (displayln (apply ~a #:separator " " 
                    (map (lambda (p) (if (path? p) (path->string p) p)) 
                         (cons exe args))))
  (flush-output)
  (case dry-run
    [(ok) #t]
    [(fail) #f]
    [(error) (error "error")]
    [(stuck) (semaphore-wait (make-semaphore))]
    [(frozen) (break-enabled #f) (semaphore-wait (make-semaphore))]
    [else
     (apply system* exe args)]))

(define (ssh-script host port user server-port kind . cmds)
  (for/and ([cmd (in-list cmds)])
    (when cmd (display-time))
    (or (not cmd)
        (if (and (equal? host "localhost")
                 (not user))
            (apply system*/show cmd)
            (apply system*/show ssh 
                   "-p" (~a port)
                   ;; create tunnel to connect back to server:
                   "-R" (~a server-port ":localhost:" server-port)
                   (if user 
                       (~a user "@" host)
                       host)
                   (if (eq? kind 'unix)
                       ;; ssh needs an extra level of quoting
                       ;;  relative to sh:
                       (for/list ([arg (in-list cmd)])
                         (~a "'" 
                             (regexp-replace* #rx"'" arg "'\"'\"'")
                             "'"))
                       ;; windows quoting built into `cmd' aready
                       cmd))))))

(define (q s)
  (~a "\"" s "\""))

(define (qq l kind)
  (case kind
    [(unix macosx)
     (~a "'"
         (apply ~a #:separator " " (map q l))
         "'")]
    [(windows windows/bash)
     (~a "\""
         (apply 
          ~a #:separator " " 
          (for/list ([i (in-list l)])
            (~a "\\\""
                i
                ;; A backslash is literal unless followed by a
                ;; quote. If `i' ends in backslashes, they
                ;; must be doubled, because the \" added to
                ;; the end will make them treated as escapes.
                (let ([m (regexp-match #rx"\\\\*$" i)])
                  (car m))
                "\\\"")))
         "\"")]))

(define (shell-protect s kind)
  (case kind
    [(windows/bash)
     ;; Protect Windows arguments to go through bash, where
     ;; unquoted backslashes must be escaped, but quotes are effectively
     ;; preserved by the shell, and quoted backslashes should be left
     ;; alone; also, "&&" must be quoted to avoid parsing by bash
     (regexp-replace* "&&"
                      (list->string
                       ;; In practice, the following loop is likely to
                       ;; do nothing, because constructed command lines
                       ;; tend to have only quoted backslashes.
                       (let loop ([l (string->list s)] [in-quote? #f])
                         (cond
                          [(null? l) null]
                          [(and (equal? #\\ (car l))
                                (not in-quote?))
                           (list* #\\ #\\ (loop (cdr l) #f))]
                          [(and in-quote?
                                (equal? #\\ (car l))
                                (pair? (cdr l))
                                (or (equal? #\" (cadr l))
                                    (equal? #\\ (cadr l))))
                           (list* #\\ (cadr l) (loop (cddr l) #t))]
                          [(equal? #\" (car l))
                           (cons #\" (loop (cdr l) (not in-quote?)))]
                          [else
                           (cons (car l) (loop (cdr l) in-quote?))])))
                      "\"\\&\\&\"")]
    [else s]))

(define (client-args c server server-port kind readme)
  (define desc (client-name c))
  (define pkgs (let ([l (get-opt c '#:pkgs)])
                 (if l
                     (apply ~a #:separator " " l)
                     default-pkgs)))
  (define doc-search (choose-doc-search c default-doc-search))
  (define dist-name (or (get-opt c '#:dist-name)
                        default-dist-name))
  (define dist-base (or (get-opt c '#:dist-base)
                        default-dist-base))
  (define dist-dir (or (get-opt c '#:dist-dir)
                       default-dist-dir))
  (define dist-suffix (get-opt c '#:dist-suffix ""))
  (define dist-catalogs (choose-catalogs c '("")))
  (define sign-identity (get-opt c '#:sign-identity ""))
  (define release? (get-opt c '#:release? default-release?))
  (define source? (get-opt c '#:source? default-source?))
  (define versionless? (get-opt c '#:versionless? default-versionless?))
  (define source-pkgs? (get-opt c '#:source-pkgs? source?))
  (define source-runtime? (get-opt c '#:source-runtime? source?))
  (define mac-pkg? (get-opt c '#:mac-pkg? #f))
  (define install-name (get-opt c '#:install-name (if release? 
                                                      "" 
                                                      snapshot-install-name)))
  (define build-stamp (get-opt c '#:build-stamp (if release?
                                                    ""
                                                    (current-stamp))))
  (~a " SERVER=" server
      " SERVER_PORT=" server-port
      " PKGS=" (q pkgs)
      " DOC_SEARCH=" (q doc-search)
      " DIST_DESC=" (q desc)
      " DIST_NAME=" (q dist-name)
      " DIST_BASE=" dist-base
      " DIST_DIR=" dist-dir
      " DIST_SUFFIX=" (q dist-suffix)
      " DIST_CATALOGS_q=" (qq dist-catalogs kind)
      " SIGN_IDENTITY=" (q sign-identity)
      " INSTALL_NAME=" (q install-name)
      " BUILD_STAMP=" (q build-stamp)
      " RELEASE_MODE=" (if release? "--release" (q ""))
      " SOURCE_MODE=" (if source-runtime? "--source" (q ""))
      " VERSIONLESS_MODE=" (if versionless? "--versionless" (q ""))
      " PKG_SOURCE_MODE=" (if source-pkgs?
                              (q "--source --no-setup")
                              (q ""))
      " MAC_PKG_MODE=" (if mac-pkg? "--mac-pkg" (q ""))
      " UPLOAD=http://" server ":" server-port "/upload/"
      " README=http://" server ":" server-port "/" (q (file-name-from-path readme))))

(define (unix-build c platform host port user server server-port repo clean? pull? readme)
  (define dir (get-path-opt c '#:dir "build/plt" #:localhost (current-directory)))
  (define (sh . args)
    (list "/bin/sh" "-c" (apply ~a args)))
  (define j (or (get-opt c '#:j) 1))
  (ssh-script
   host port user
   server-port
   'unix
   (and clean?
        (sh "rm -rf  " (q dir)))
   (sh "if [ ! -d " (q dir) " ] ; then"
       " git clone " (q repo) " " (q dir) " ; "
       "fi")
   (and pull?
        (sh "cd " (q dir) " ; "
            "git pull"))
   (sh "cd " (q dir) " ; "
       "make -j " j " client"
       (client-args c server server-port 'unix readme)
       " JOB_OPTIONS=\"-j " j "\""
       " CONFIGURE_ARGS_qq=" (qq (get-opt c '#:configure null) 'unix))))

(define (windows-build c platform host port user server server-port repo clean? pull? readme)
  (define dir (get-path-opt c '#:dir "build\\plt" #:localhost (current-directory)))
  (define bits (or (get-opt c '#:bits) 64))
  (define vc (or (get-opt c '#:vc)
                 (if (= bits 32)
                     "x86"
                     "x86_amd64")))
  (define j (or (get-opt c '#:j) 1))
  (define (cmd . args) 
    (list "cmd" "/c" (shell-protect (apply ~a args) platform)))
  (ssh-script
   host port user
   server-port
   platform
   (and clean?
        (cmd "IF EXIST " (q dir) " rmdir /S /Q " (q dir)))
   (cmd "IF NOT EXIST " (q dir) " git clone " (q repo) " " (q dir))
   (and pull?
        (cmd "cd " (q dir)
             " && git pull"))
   (cmd "cd " (q dir)
        " && racket\\src\\worksp\\msvcprep.bat " vc
        " && nmake win32-client" 
        " JOB_OPTIONS=\"-j " j "\""
        (client-args c server server-port platform readme))))

(define (client-build c)
  (define host (or (get-opt c '#:host)
                   "localhost"))
  (define port (or (get-opt c '#:port)
                   22))
  (define user (get-opt c '#:user))
  (define server (or (get-opt c '#:server)
                     default-server))
  (define server-port (or (get-opt c '#:server-port)
                          default-server-port))
  (define repo (or (get-opt c '#:repo)
                   (~a "http://" server ":" server-port "/.git")))
  (define clean? (get-opt c '#:clean? default-clean? #:localhost #f))
  (define pull? (get-opt c '#:pull? #t #:localhost #f))

  (define readme-txt (let ([rdme (get-opt c '#:readme make-readme)])
                       (if (string? rdme)
                           rdme
                           (rdme (add-defaults c
                                               '#:release? default-release?
                                               '#:source? default-source?
                                               '#:versionless? default-versionless?
                                               '#:pkgs (string-split default-pkgs)
                                               '#:install-name (if (get-opt c '#:release? default-release?)
                                                                   ""
                                                                   snapshot-install-name)
                                               '#:build-stamp (if (get-opt c '#:release? default-release?)
                                                                  ""
                                                                  (current-stamp)))))))
  (make-directory* (build-path "build" "readmes"))
  (define readme (make-temporary-file
                  "README-~a"
                  #f
                  (build-path "build" "readmes")))
  (call-with-output-file*
   readme
   #:exists 'truncate
   (lambda (o)
     (display readme-txt o)
     (unless (regexp-match #rx"\n$" readme-txt)
       ;; ensure a newline at the end:
       (newline o))))

  (define platform (or (get-opt c '#:platform) (system-type)))

  (begin0

   ((case platform
      [(unix macosx) unix-build]
      [else windows-build])
    c platform host port user server server-port repo clean? pull? readme)

   (delete-file readme)))

;; ----------------------------------------

(define stop? #f)

(define failures (make-hasheq))
(define (record-failure name)
  ;; relies on atomicity of `eq?'-based hash table:
  (hash-set! failures (string->symbol name) #t))

(define (limit-and-report-failure c timeout-factor
                                  shutdown report-fail
                                  thunk)
  (define cust (make-custodian))
  (define timeout (or (get-opt c '#:timeout)
                      (* 30 60)))
  (define orig-thread (current-thread))
  (define timeout? #f)
  (begin0
   (parameterize ([current-custodian cust])
     (thread (lambda ()
               (sleep (* timeout-factor timeout))
               (eprintf "timeout for ~s\n" (client-name c))
               ;; try nice interrupt, first:
               (set! timeout? #t)
               (break-thread orig-thread)
               (sleep 1)
               ;; force quit:
               (report-fail)
               (shutdown)))
     (with-handlers ([exn? (lambda (exn)
                             (when (exn:break? exn)
                               ;; This is useful only when everything is
                               ;; sequential, which is the only time that
                               ;; we'll get break events that aren't timeouts:
                               (unless timeout?
                                 (set! stop? #t)))
                             (log-error "~a failed..." (client-name c))
                             (log-error (exn-message exn))
                             (report-fail)
                             #f)])
       (thunk)))
   (custodian-shutdown-all cust)))

(define (client-thread c all-seq? proc)
  (unless stop?
    (define log-dir (build-path "build" "log"))
    (define log-file (build-path log-dir (client-name c)))
    (make-directory* log-dir)
    (printf "Logging build: ~a\n" log-file)
    (flush-output)
    (define cust (make-custodian))
    (define (go shutdown)
      (define p (open-output-file log-file
                                  #:exists 'truncate/replace))
      (file-stream-buffer-mode p 'line)
      (define (report-fail)
        (record-failure (client-name c))
        (printf "Build FAILED for ~s\n" (client-name c)))
      (unless (parameterize ([current-output-port p]
                             [current-error-port p])
                (proc shutdown report-fail))
        (report-fail))
      (display-time))
    (cond
     [all-seq? 
      (go (lambda () (exit 1)))
      (thread void)]
     [else
      (parameterize ([current-custodian cust])
        (thread
         (lambda ()
           (go (lambda ()
                 (custodian-shutdown-all cust))))))])))

;; ----------------------------------------

(define start-seconds (current-seconds))
(display-time)

(void
 (sync
  (let loop ([config config]
             [all-seq? #t] ; Ctl-C handling is better if nothing is in parallel
             [opts (hasheq)])
    (cond
     [stop? (thread void)]
     [else
      (case (site-config-tag config)
        [(parallel)
         (define new-opts (merge-options opts config))
         (define ts
           (map (lambda (c) (loop c #f new-opts))
                (get-content config)))
         (thread
          (lambda ()
            (for ([t (in-list ts)])
              (sync t))))]
        [(sequential)
         (define new-opts (merge-options opts config))
         (define (go)
           (for-each (lambda (c) (sync (loop c all-seq? new-opts)))
                     (get-content config)))
         (if all-seq?
             (begin (go) (thread void))
             (thread go))]
        [else 
         (define c (merge-options opts config))
         (client-thread
          c
          all-seq?
          (lambda (shutdown report-fail)
            (limit-and-report-failure
             c 2 shutdown report-fail
             (lambda ()
               (sleep (get-opt c '#:pause-before 0))
               ;; start client, if a VM:
               (start-client c (or (get-opt c '#:max-vm) 1))
               ;; catch failure in build step proper, so we
               ;; can more likely stop the client:
               (begin0
                (limit-and-report-failure
                 c 1 shutdown report-fail
                 (lambda () (client-build c)))
                ;; stop client, if a VM:
                (stop-client c)
                (sleep (get-opt c '#:pause-after 0)))))))])]))))

(display-time)
(define end-seconds (current-seconds))

(unless stop?
  (let ([opts (merge-options (hasheq) config)])
    (let ([to-email (get-opt opts '#:email-to null)])
      (unless (null? to-email)
        (printf "Sending report to ~a\n" (apply ~a to-email #:separator ", "))
        (send-email to-email (lambda (key def)
                               (get-opt opts key def))
                    (get-opt opts '#:build-stamp (current-stamp))
                    start-seconds end-seconds
                    (hash-map failures (lambda (k v) (symbol->string k))))
        (display-time)))))
