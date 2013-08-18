#lang racket/base
(require racket/cmdline
         racket/system
         racket/port
         racket/format
         racket/file
         racket/string
         racket/path
         (only-in "config.rkt"
                  current-mode
                  site-config?
                  site-config-tag site-config-options site-config-content
                  current-stamp)
         "url-options.rkt"
         "display-time.rkt"
         "readme.rkt")

;; See "config.rkt" for an overview.

;; ----------------------------------------

(define default-release? #f)
(define default-clean? #f)

(define snapshot-install-name "snapshot")

(define-values (config-file config-mode
                            default-server default-pkgs default-doc-search
                            default-dist-name default-dist-base default-dist-dir)
  (command-line
   #:once-each
   [("--release") "Create release-mode installers"
    (set! default-release? #t)]
   [("--clean") "Erase client directories before building"
    (set! default-clean? #t)]
   #:args (config-file config-mode 
                       server pkgs doc-search
                       dist-name dist-base dist-dir)
   (values config-file config-mode
           server pkgs doc-search
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

(define VBoxManage (find-executable-path "VBoxManage"))
(define use-headless? #t)

(define (system*/show exe . args)
  (displayln (apply ~a #:separator " " 
                    (map (lambda (p) (if (path? p) (path->string p) p)) 
                         (cons exe args))))
  (flush-output)
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
    (flush-output)
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
    (flush-output)
    (vbox-control vbox "savestate")
    (unless (eq? (vbox-state vbox) 'saved)
      (error 'start-client "virtual machine isn't in the expected saved state: ~s" c))))

;; ----------------------------------------

(define scp (find-executable-path "scp"))
(define ssh (find-executable-path "ssh"))

(define (ssh-script host port user kind . cmds)
  (for/and ([cmd (in-list cmds)])
    (when cmd (display-time))
    (or (not cmd)
        (if (and (equal? host "localhost")
                 (not user))
            (apply system*/show cmd)
            (apply system*/show ssh 
                   "-p" (~a port)
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
                       ;; windows quiting built into `cmd' aready
                       cmd))))))

(define (q s)
  (~a "\"" s "\""))

(define (qq l kind)
  (case kind
    [(unix) (~a "'"
                (apply ~a #:separator " " (map q l))
                "'")]
    [(windows) (~a "\""
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

(define (client-args c server kind readme)
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
  (define release? (get-opt c '#:release? default-release?))
  (define source? (get-opt c '#:source? #f))
  (define source-pkgs? (get-opt c '#:source-pkgs? source?))
  (define source-runtime? (get-opt c '#:source-runtime? source?))
  (define install-name (get-opt c '#:install-name (if release? 
                                                      "" 
                                                      snapshot-install-name)))
  (define build-stamp (get-opt c '#:build-stamp (if release?
                                                    ""
                                                    (current-stamp))))
  (~a " SERVER=" server
      " PKGS=" (q pkgs)
      " DOC_SEARCH=" (q doc-search)
      " DIST_DESC=" (q desc)
      " DIST_NAME=" (q dist-name)
      " DIST_BASE=" dist-base
      " DIST_DIR=" dist-dir
      " DIST_SUFFIX=" (q dist-suffix)
      " DIST_CATALOGS_q=" (qq dist-catalogs kind)
      " INSTALL_NAME=" (q install-name)
      " BUILD_STAMP=" (q build-stamp)
      " RELEASE_MODE=" (if release? "--release" (q ""))
      " SOURCE_MODE=" (if source-runtime? "--source" (q ""))
      " PKG_SOURCE_MODE=" (if source-pkgs?
                              (q "--source --no-setup")
                              (q ""))
      " README=" (q (file-name-from-path readme))))

(define (unix-build c host port user server repo clean? pull? readme)
  (define dir (get-path-opt c '#:dir "build/plt" #:localhost (current-directory)))
  (define (sh . args)
    (list "/bin/sh" "-c" (apply ~a args)))
  (define j (or (get-opt c '#:j) 1))
  (ssh-script
   host port user
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
       (client-args c server 'unix readme)
       " JOB_OPTIONS=\"-j " j "\""
       " CONFIGURE_ARGS_qq=" (qq (get-opt c '#:configure null) 'unix))))

(define (windows-build c host port user server repo clean? pull? readme)
  (define dir (get-path-opt c '#:dir "build\\plt" #:localhost (current-directory)))
  (define bits (or (get-opt c '#:bits) 64))
  (define vc (or (get-opt c '#:vc)
                 (if (= bits 32)
                     "x86"
                     "x64")))
  (define j (or (get-opt c '#:j) 1))
  (define (cmd . args) 
    (list "cmd" "/c" (apply ~a args)))
  (ssh-script
   host port user
   'windows
   (and clean?
        (cmd "IF EXIST " (q dir) " rmdir /S /Q " (q dir)))
   (cmd "IF NOT EXIST " (q dir) " git clone " (q repo) " " (q dir))
   (and pull?
        (cmd "cd " (q dir)
             " && git pull"))
   (cmd "cd " (q dir)
        " && \"c:\\Program Files" (if (= bits 64) " (x86)" "") "\\Microsoft Visual Studio 9.0\\vc\\vcvarsall.bat\""
        " " vc
        " && nmake win32-client" 
       " JOB_OPTIONS=\"-j " j "\""
        (client-args c server 'windows readme))))

(define (client-build c)
  (define host (or (get-opt c '#:host)
                   "localhost"))
  (define port (or (get-opt c '#:port)
                   22))
  (define user (get-opt c '#:user))
  (define server (or (get-opt c '#:server)
                     default-server))
  (define repo (or (get-opt c '#:repo)
                   (~a "http://" server ":9440/.git")))
  (define clean? (get-opt c '#:clean? default-clean? #:localhost #f))
  (define pull? (get-opt c '#:pull? #t #:localhost #f))

  (define readme-txt (let ([rdme (get-opt c '#:readme make-readme)])
                       (if (string? rdme)
                           rdme
                           (rdme (add-defaults c
                                               '#:release? default-release?
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

  (begin0

   ((case (or (get-opt c '#:platform) (system-type))
      [(unix macosx) unix-build]
      [else windows-build])
    c host port user server repo clean? pull? readme)

   (delete-file readme)))

;; ----------------------------------------

(define stop? #f)

(define (limit-and-report-failure c timeout-factor thunk)
  (unless stop?
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
                 (custodian-shutdown-all cust)))
       (with-handlers ([exn? (lambda (exn)
                               (when (exn:break? exn) 
                                 (unless timeout?
                                   (set! stop? #t)))
                               (log-error "~a failed..." (client-name c))
                               (log-error (exn-message exn)))])
         (thunk)))
     (custodian-shutdown-all cust))))

(define (client-thread c sequential? thunk)
  (unless stop?
    (define log-dir (build-path "build" "log"))
    (define log-file (build-path log-dir (client-name c)))
    (make-directory* log-dir)
    (printf "Logging build: ~a\n" log-file)
    (flush-output)
    (define (go)
      (define p (open-output-file log-file
                                  #:exists 'truncate/replace))
      (file-stream-buffer-mode p 'line)
      (unless (parameterize ([current-output-port p]
                             [current-error-port p])
                (thunk))
        (printf "Build FAILED for ~s\n" (client-name c))))
    (cond
     [sequential? (go) (thread void)]
     [else (thread go)])))

;; ----------------------------------------

(display-time)

(void
 (let loop ([config config]
            [mode 'sequential]
            [opts (hasheq)])
   (unless stop?
     (case (site-config-tag config)
       [(parallel sequential)
        (define new-opts (merge-options opts config))
        (define ts
          (map (lambda (c) (loop c
                                 (site-config-tag config)
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
              (sleep (get-opt c '#:pause-before 0))
              ;; start client, if a VM:
              (start-client c (or (get-opt c '#:max-vm) 1))
              ;; catch failure in build step proper, so we
              ;; can more likely stop the client:
              (begin0
               (limit-and-report-failure
                c 1
                (lambda () (client-build c)))
               ;; stop client, if a VM:
               (stop-client c)
               (sleep (get-opt c '#:pause-after 0)))))))]))))

(display-time)
