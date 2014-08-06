#lang racket/base
(require racket/cmdline
         racket/file
         racket/port
         racket/format
         racket/date
         racket/list
         racket/set
         racket/string
         racket/runtime-path
         net/url
         pkg/lib
         file/untgz
         file/tar
         file/gzip
         remote-shell/vbox
         remote-shell/ssh
         web-server/servlet-env
         (only-in scribble/html a td tr #%top)
         "download.rkt"
         "union-find.rkt"
         "thread.rkt"
         "status.rkt"
         "extract-doc.rkt"
         "summary.rkt")

(provide vbox-vm
         build-pkgs
         steps-in)

(define-runtime-path pkg-list-rkt "pkg-list.rkt")
(define-runtime-path pkg-adds-rkt "pkg-adds.rkt")

;; ----------------------------------------

;; Builds all packages from a given catalog and using a given snapshot.
;; The build of each package is isolated through a virtual machine,
;; and the result is both a set of built packages and a complete set
;; of documentation.
;;
;; To successfully build, a package must
;;   - install without error
;;   - correctly declare its dependencies (but may work, anyway,
;;     if build order happens to accomodate)
;;   - depend on packages that build successfully on their own
;;   - refer only to other packages in the snapshot and catalog
;;     (and, in particular, must not use PLaneT packages)
;;   - build without special system libraries (i.e., beyond the ones
;;     needed by `racket/draw`)
;;
;; A successful build not not require that its declaraed dependencies
;; are complete if the needed packages end up installed, anyway, but
;; the declaraed dependencies are checked.
;;
;; Even when a build is unsuccessful, any documentation that is built
;; along the way is extracted, if possible.
;;
;; To do:
;;  - tier-based selection of packages on conflict
;;  - support for running tests

(struct vm (host user dir name init-snapshot installed-snapshot))

;; Each VM must provide at least an ssh server and `tar`, it must have
;; any system libraries installed that are needed for building
;; (typically the libraries needed by `racket/draw`), and the intent
;; is that it is otherwise isolated (e.g., no network connection
;; except to the host)
(define (vbox-vm
         ;; VirtualBox VM name:
         #:name name
         ;; IP address of VM (from host):
         #:host host
         ;; User for ssh login to VM:
         #:user [user "racket"]
         ;; Working directory on VM:
         #:dir [dir "/home/racket/build-pkgs"]
         ;; Name of a clean starting snapshot in the VM:
         #:init-shapshot [init-snapshot "init"]
         ;; An "installed" snapshot is created after installing Racket
         ;; and before building any package:
         #:installed-shapshot [installed-snapshot "installed"])
  (unless (complete-path? dir)
    (error 'vbox-vm "need a complete path for #:dir"))
  (vm host user dir name init-snapshot installed-snapshot))

;; The build steps:
(define all-steps-in-order
  (list
   ;; Download installer from snapshot site:
   'download
   ;; Archive catalogs, downlowning the catalog and all
   ;; packages to the working directory:
   'archive
   ;; Install into each VM:
   'install
   ;; Build packages that have changed:
   'build
   ;; Extract and assemble documentation:
   'docs
   ;; Build a result-summary file and web page:
   'summary
   ;; Assemble web-friendly pieces to an archive:
   'site))

;; Return the subset of steps with `start` through `end` inclusive:
(define (steps-in start end)
  (define l (member start all-steps-in-order))
  (if l
      (let ([l (member end (reverse l))])
        (if l
            (reverse l)
            (if (member end all-steps-in-order)
                (error 'steps-in "steps out of order: ~e and: ~e" start end)
                (error 'steps-in "bad ending step: ~e" end))))
      (error 'steps-in "bad starting step: ~e" start)))

(define (build-pkgs 
         ;; Besides a running Racket, the host machine must provide
         ;; `ssh`, `scp`, and `VBoxManage`.

         ;; All local state is here, where state from a previous
         ;; run is used to work incrementally:
         #:work-dir [given-work-dir (current-directory)]
         ;; Directory content:
         ;;
         ;;   "installer" --- directly holding installer downloaded
         ;;     from the snapshot site
         ;;
         ;;   "install-list.rktd" --- list of packages found in
         ;;     the installation
         ;;   "install-adds.rktd" --- table of docs, libs, etc.
         ;;     in the installation (to detect conflicts)
         ;;   "install-doc.tgz" --- copy of installation's docs
         ;;
         ;;   "server/archive" plus "state.sqlite" --- archived
         ;;     packages, taken from the snapshot site plus additional
         ;;     specified catalogs
         ;;
         ;;   "server/built" --- built packages
         ;;     For a package P:
         ;;      * "pkgs/P.orig-CHECKSUM" matching archived catalog
         ;;         + "pkgs/P.zip"
         ;;         + "P.zip.CHECKSUM"
         ;;        => up-to-date and successful,
         ;;           "docs/P-adds.rktd" listing of docs, exes, etc., and
         ;;           "success/P.txt" records success;
         ;;           "install/P.txt" records installation
         ;;           "deps/P.txt" record dependency-checking failure;
         ;;      * pkgs/P.orig-CHECKSUM matching archived catalog
         ;;         + fail/P.txt
         ;;        => up-to-date and failed;
         ;;           "install/P.txt" may record installation success
         ;;
         ;;   "dumpster" --- saved builds of failed packages if the
         ;;     package at least installs; maybe the attempt built
         ;;     some documentation
         ;;
         ;;   "doc" --- unpacked docs with non-conflicting
         ;;     packages installed
         ;;   "all-doc.tgz" --- "doc", still packed
         ;;
         ;;   "summary.rktd" --- summary of build results, a hash
         ;;     table mapping each package name to another hash table
         ;;     with the following keys:
         ;;       'success-log --- #f or relative path
         ;;       'failure-log --- #f or relative path
         ;;       'dep-failure-log --- #f or relative path
         ;;       'docs --- list of one of
         ;;                  * (docs/none name)
         ;;                  * (docs/main name path)
         ;;       'conflict-log --- #f, relative path, or
         ;;                         (conflicts/indirect path)
         ;;   "index.html" (and "robots.txt", etc.) --- summary in
         ;;     web-page form
         ;;
         ;; A package is rebuilt if its checksum changes or if one of
         ;; its declared dependencies changes.

         ;; URL to provide the installer and pre-built packages:
         #:snapshot-url snapshot-url
         ;; Name of platform for installer to get from snapshot:
         #:installer-platform-name installer-platform-name

         ;; VirtualBox VMs (created by `vbox-vm`), at least one:
         #:vms vms

         ;; Catalogs of packages to build (via an archive):
         #:pkg-catalogs [pkg-catalogs (list "http://pkgs.racket-lang.org/")]

         ;; The Racket version to use in queries to archived catalogs;
         ;; this version should be consistent with `snapshot-url`.
         #:pkgs-for-version [pkgs-for-version (version)]

         ;; Extra packages to install within an installation so that
         ;; they're treated like packages included in the installer;
         ;; these should be built packages (normally from the snapshot
         ;; site), or else the generated build packages will not work
         ;; right (especially when using multiple VMs):
         #:extra-packages [extra-packages null]

         ;; Steps that you want to include; you can skip steps
         ;; at the beginning if you know they're already done, and
         ;; you can skip tests at the end if you don't want them:
         #:steps [steps (steps-in 'download 'summary)]

         ;; Omit specified packages from the summary:
         #:summary-omit-pkgs [summary-omit-pkgs null]

         ;; Timeout in seconds for any one package or step:
         #:timeout [timeout 600]

         ;; Building more than one package at a time case be faster,
         ;; but it risks success when a build should have failed due
         ;; to missing dependencies, and it risks corruption due to
         ;; especially broken or nefarious packages:
         #:max-build-together [max-build-together 1]         

         ;; Port to use on host machine for catalog server:
         #:server-port [server-port 18333])

  (unless (and (list? vms)
               ((length vms) . >= . 1)
               (andmap vm? vms))
    (error 'build-pkgs "expected a non-empty list of `vm`s"))

  (for ([step (in-list steps)])
    (unless (member step all-steps-in-order)
      (error 'build-pkgs "bad step: ~e" step)))
  
  (define skip-download? (not (member 'download steps)))
  (define skip-install? (not (member 'install steps)))
  (define skip-archive? (not (member 'archive steps)))
  (define skip-build? (not (member 'build steps)))
  (define skip-docs? (not (member 'docs steps)))
  (define skip-summary? (not (member 'summary steps)))
  (define skip-site? (not (member 'site steps)))

  (define work-dir (path->complete-path given-work-dir))
  (define installer-dir (build-path work-dir "installer"))
  (define server-dir (build-path work-dir "server"))
  (define archive-dir (build-path server-dir "archive"))
  (define state-file (build-path work-dir "state.sqlite"))

  (define built-dir (build-path server-dir "built"))
  (define built-pkgs-dir (build-path built-dir "pkgs/"))
  (define built-catalog-dir (build-path built-dir "catalog"))
  (define fail-dir (build-path built-dir "fail"))
  (define success-dir (build-path built-dir "success"))
  (define install-success-dir (build-path built-dir "install"))
  (define deps-fail-dir (build-path built-dir "deps"))

  (define dumpster-dir (build-path work-dir "dumpster"))
  (define dumpster-pkgs-dir (build-path dumpster-dir "pkgs/"))
  (define dumpster-adds-dir (build-path dumpster-dir "adds"))

  (define doc-dir (build-path work-dir "doc"))

  (define (txt s) (~a s ".txt"))

  (define snapshot-catalog
    (url->string
     (combine-url/relative (string->url snapshot-url)
                           "catalog/")))

  (make-directory* work-dir)

  ;; ----------------------------------------

  (define (q s)
    (~a "\"" s "\""))

  (define (at-vm vm dest)
    (at-remote (vm-remote vm) dest))
  
  (define (cd-racket vm) (~a "cd " (q (vm-dir vm)) "/racket"))

  (define (vm-remote vm)
    (remote #:host (vm-host vm)
            #:user (vm-user vm)
            #:env (list (cons "PLTUSERHOME"
                              (~a (vm-dir vm) "/user")))
            #:timeout timeout
            #:remote-tunnels (list (cons server-port server-port))))

  ;; ----------------------------------------
  (define installer-table-path (build-path work-dir "table.rktd"))
  (unless skip-download?
    (status "Getting installer table\n")
    (define table (call/input-url
                   (combine-url/relative (string->url snapshot-url)
                                         "installers/table.rktd")
                   get-pure-port
                   (lambda (i) (read i))))
    (call-with-output-file*
     installer-table-path
     #:exists 'truncate/replace
     (lambda (o) (write table o) (newline o))))

  (define installer-name (hash-ref
                          (call-with-input-file*
                           installer-table-path
                           read)
                          installer-platform-name))
  (substatus "Installer is ~a\n" installer-name)

  ;; ----------------------------------------
  (unless skip-download?
    (status "Downloading installer ~a\n" installer-name)
    (download-installer snapshot-url installer-dir installer-name substatus))

  ;; ----------------------------------------
  (unless skip-archive?
    (status "Archiving packages from\n")
    (show-list (cons snapshot-catalog pkg-catalogs))
    (make-directory* archive-dir)
    (parameterize ([current-pkg-lookup-version pkgs-for-version])
      (pkg-catalog-archive archive-dir
                           (cons snapshot-catalog pkg-catalogs)
                           #:state-catalog state-file
                           #:relative-sources? #t
                           #:package-exn-handler (lambda (name exn)
                                                   (log-error "~a\nSKIPPING ~a"
                                                              (exn-message exn)
                                                              name)))))

  (define snapshot-pkg-names
    (parameterize ([current-pkg-catalogs (list (string->url snapshot-catalog))])
      (get-all-pkg-names-from-catalogs)))

  (define all-pkg-names
    (parameterize ([current-pkg-catalogs (list (path->url (build-path archive-dir "catalog")))])
      (get-all-pkg-names-from-catalogs)))

  (define pkg-details
    (parameterize ([current-pkg-catalogs (list (path->url (build-path archive-dir "catalog")))])
      (get-all-pkg-details-from-catalogs)))

  ;; ----------------------------------------
  (status "Starting server at locahost:~a for ~a\n" server-port archive-dir)

  (define server
    (thread
     (lambda ()
       (serve/servlet
        (lambda args #f)
        #:command-line? #t
        #:listen-ip "localhost"
        #:extra-files-paths (list server-dir)
        #:servlet-regexp #rx"$." ; never match
        #:port server-port))))
  (sync (system-idle-evt))

  ;; ----------------------------------------
  (define (install vm #:one-time? [one-time? #f])
    (status "Starting VM ~a\n" (vm-name vm))
    (stop-vbox-vm (vm-name vm))
    (restore-vbox-snapshot (vm-name vm) (vm-init-snapshot vm))

    (dynamic-wind
     (lambda () (start-vbox-vm (vm-name vm)))
     (lambda ()
       (define rt (vm-remote vm))
       (make-sure-remote-is-ready rt)
       ;; ----------------------------------------
       (status "Fixing time at ~a\n" (vm-name vm))
       (ssh rt "sudo date --set=" (q (parameterize ([date-display-format 'rfc2822])
                                       (date->string (seconds->date (current-seconds)) #t))))

       ;; ----------------------------------------
       (define there-dir (vm-dir vm))
       (status "Preparing directory ~a\n" there-dir)
       (ssh rt "rm -rf " (~a (q there-dir) "/*"))
       (ssh rt "mkdir -p " (q there-dir))
       (ssh rt "mkdir -p " (q (~a there-dir "/user")))
       (ssh rt "mkdir -p " (q (~a there-dir "/built")))
       
       (scp rt (build-path installer-dir installer-name) (at-vm vm there-dir))
       
       (ssh rt "cd " (q there-dir) " && " " sh " (q installer-name) " --in-place --dest ./racket")
       
       ;; VM-side helper modules:
       (scp rt pkg-adds-rkt (at-vm vm (~a there-dir "/pkg-adds.rkt")))
       (scp rt pkg-list-rkt (at-vm vm (~a there-dir "/pkg-list.rkt")))

       ;; ----------------------------------------
       (status "Setting catalogs at ~a\n" (vm-name vm))
       (ssh rt (cd-racket vm)
            " && bin/raco pkg config -i --set catalogs "
            " http://localhost:" (~a server-port) "/built/catalog/"
            " http://localhost:" (~a server-port) "/archive/catalog/")

       ;; ----------------------------------------
       (unless (null? extra-packages)
         (status "Extra package installs at ~a\n" (vm-name vm))
         (ssh rt (cd-racket vm)
              " && bin/raco pkg install -i --auto"
              " " (apply ~a #:separator " " extra-packages)))

       (when one-time?
         ;; ----------------------------------------
         (status "Getting installed packages\n")
         (ssh rt (cd-racket vm)
              " && bin/racket ../pkg-list.rkt > ../pkg-list.rktd")
         (scp rt (at-vm vm (~a there-dir "/pkg-list.rktd"))
              (build-path work-dir "install-list.rktd"))

         ;; ----------------------------------------
         (status "Stashing installation docs\n")
         (ssh rt (cd-racket vm)
              " && bin/racket ../pkg-adds.rkt --all > ../pkg-adds.rktd")
         (ssh rt (cd-racket vm)
              " && tar zcf ../install-doc.tgz doc")
         (scp rt (at-vm vm (~a there-dir "/pkg-adds.rktd"))
              (build-path work-dir "install-adds.rktd"))
         (scp rt (at-vm vm (~a there-dir "/install-doc.tgz"))
              (build-path work-dir "install-doc.tgz")))
       
       (void))
     (lambda ()
       (stop-vbox-vm (vm-name vm))))

    ;; ----------------------------------------
    (status "Taking installation snapshopt\n")
    (when (exists-vbox-snapshot? (vm-name vm) (vm-installed-snapshot vm))
      (delete-vbox-snapshot (vm-name vm) (vm-installed-snapshot vm)))
    (take-vbox-snapshot (vm-name vm) (vm-installed-snapshot vm)))

  (unless skip-install?
    (install (car vms) #:one-time? #t)
    (map install (cdr vms)))
  
  ;; ----------------------------------------
  (status "Resetting ready content of ~a\n" built-pkgs-dir)

  (make-directory* built-pkgs-dir)

  (define installed-pkg-names
    (call-with-input-file* (build-path work-dir "install-list.rktd") read))

  (substatus "Total number of packages: ~a\n" (length all-pkg-names))
  (substatus "Packages installed already: ~a\n" (length installed-pkg-names))

  (define snapshot-pkgs (list->set snapshot-pkg-names))
  (define installed-pkgs (list->set installed-pkg-names))

  (define try-pkgs (set-subtract (list->set all-pkg-names)
                                 installed-pkgs))

  (define (pkg-checksum pkg) (hash-ref (hash-ref pkg-details pkg) 'checksum ""))
  (define (pkg-author pkg) (hash-ref (hash-ref pkg-details pkg) 'author ""))
  (define (pkg-checksum-file pkg) (build-path built-pkgs-dir (~a pkg ".orig-CHECKSUM")))
  (define (pkg-zip-file pkg) (build-path built-pkgs-dir (~a pkg ".zip")))
  (define (pkg-zip-checksum-file pkg) (build-path built-pkgs-dir (~a pkg ".zip.CHECKSUM")))
  (define (pkg-failure-dest pkg) (build-path fail-dir (txt pkg)))

  (define failed-pkgs
    (for/set ([pkg (in-list all-pkg-names)]
              #:when
              (let ()
                (define checksum (pkg-checksum pkg))
                (define checksum-file (pkg-checksum-file pkg))
                (and (file-exists? checksum-file)
                     (equal? checksum (file->string checksum-file))
                     (not (set-member? installed-pkgs pkg))
                     (file-exists? (pkg-failure-dest pkg)))))
      pkg))

  (define changed-pkgs
    (for/set ([pkg (in-list all-pkg-names)]
              #:unless
              (let ()
                (define checksum (pkg-checksum pkg))
                (define checksum-file (pkg-checksum-file pkg))
                (and (file-exists? checksum-file)
                     (equal? checksum (file->string checksum-file))
                     (or (set-member? installed-pkgs pkg)
                         (file-exists? (pkg-failure-dest pkg))
                         (and
                          (file-exists? (pkg-zip-file pkg))
                          (file-exists? (pkg-zip-checksum-file pkg)))))))
      pkg))

  (define (pkg-deps pkg)
    (map (lambda (dep) 
           (define d (if (string? dep) dep (car dep)))
           (if (equal? d "racket") "base" d))
         (hash-ref (hash-ref pkg-details pkg) 'dependencies null)))

  (define update-pkgs
    (let loop ([update-pkgs changed-pkgs])
       (define more-pkgs
         (for/set ([pkg (in-set try-pkgs)]
                   #:when (and (not (set-member? update-pkgs pkg))
                               (for/or ([dep (in-list (pkg-deps pkg))])
                                 (set-member? update-pkgs dep))))
           pkg))
       (if (set-empty? more-pkgs)
           update-pkgs
           (loop (set-union more-pkgs update-pkgs)))))

  ;; Remove any ".zip[.CHECKSUM]" for packages that need to be built
  (for ([pkg (in-set update-pkgs)])
    (define checksum-file (pkg-checksum-file pkg))
    (when (file-exists? checksum-file) (delete-file checksum-file))
    (define zip-file (pkg-zip-file pkg))
    (when (file-exists? zip-file) (delete-file zip-file))
    (define zip-checksum-file (pkg-zip-checksum-file pkg))
    (when (file-exists? zip-checksum-file) (delete-file zip-checksum-file)))

  ;; For packages in the installation, remove any ".zip[.CHECKSUM]" and set ".orig-CHECKSUM"
  (for ([pkg (in-set installed-pkgs)])
    (define checksum-file (pkg-checksum-file pkg))
    (define zip-file (pkg-zip-file pkg))
    (define zip-checksum-file (pkg-zip-checksum-file pkg))
    (define failure-dest (pkg-failure-dest pkg))
    (when (file-exists? zip-file) (delete-file zip-file))
    (when (file-exists? zip-checksum-file) (delete-file zip-checksum-file))
    (when (file-exists? failure-dest) (delete-file failure-dest))
    (call-with-output-file*
     checksum-file
     #:exists 'truncate/replace
     (lambda (o)
       (write-string (pkg-checksum pkg) o))))

  (define need-pkgs (set-subtract (set-subtract update-pkgs installed-pkgs)
                                  failed-pkgs))

  (define cycles (make-hash)) ; for union-find

  ;; Sort needed packages based on dependencies, and accumulate cycles:
  (define need-rep-pkgs-list
    (let loop ([l (sort (set->list need-pkgs) string<?)] [seen (set)] [cycle-stack null])
      (if (null? l)
          null
          (let ([pkg (car l)])
            (cond
             [(member pkg cycle-stack)
              ;; Hit a package while processing its dependencies;
              ;; everything up to that package on the stack is
              ;; mutually dependent:
              (for ([s (in-list (member pkg (reverse cycle-stack)))])
                (union! cycles pkg s))
              (loop (cdr l) seen cycle-stack)]
             [(set-member? seen pkg)
              (loop (cdr l) seen cycle-stack)]
             [else
              (define pkg (car l))
              (define new-seen (set-add seen pkg))
              (define deps
                (for/list ([dep (in-list (pkg-deps pkg))]
                           #:when (set-member? need-pkgs dep))
                  dep))
              (define pre (loop deps new-seen (cons pkg cycle-stack)))
              (define pre-seen (set-union new-seen (list->set pre)))
              (define remainder (loop (cdr l) pre-seen cycle-stack))
              (elect! cycles pkg) ; in case of mutual dependency, follow all pre-reqs
              (append pre (cons pkg remainder))])))))

  ;; A list that contains strings and lists of strings, where a list
  ;; of strings represents mutually dependent packages:
  (define need-pkgs-list
    (let ([reps (make-hash)])
      (for ([pkg (in-set need-pkgs)])
        (hash-update! reps (find! cycles pkg) (lambda (l) (cons pkg l)) null))
      (for/list ([pkg (in-list need-rep-pkgs-list)]
                 #:when (equal? pkg (find! cycles pkg)))
        (define pkgs (hash-ref reps pkg))
        (if (= 1 (length pkgs))
            pkg
            pkgs))))

  (substatus "Packages that we need:\n")
  (show-list need-pkgs-list)

  ;; ----------------------------------------
  (status "Preparing built catalog at ~a\n" built-catalog-dir)

  (define (update-built-catalog given-pkgs)
    ;; Don't shadow anything from the catalog, even if we "built" it to
    ;; get documentation:
    (define pkgs (filter (lambda (pkg) (not (set-member? snapshot-pkgs pkg)))
                         given-pkgs))
    ;; Generate info for each now-built package:
    (define hts (for/list ([pkg (in-list pkgs)])
                  (let* ([ht (hash-ref pkg-details pkg)]
                         [ht (hash-set ht 'source (~a "../pkgs/" pkg ".zip"))]
                         [ht (hash-set ht 'checksum
                                       (file->string (build-path built-pkgs-dir
                                                                 (~a pkg ".zip.CHECKSUM"))))])
                    ht)))
    (for ([pkg (in-list pkgs)]
          [ht (in-list hts)])
      (call-with-output-file*
       (build-path built-catalog-dir "pkg" pkg)
       (lambda (o) (write ht o) (newline o))))
    (define old-all (call-with-input-file* (build-path built-catalog-dir "pkgs-all") read))
    (define all
      (for/fold ([all old-all]) ([pkg (in-list pkgs)]
                                 [ht (in-list hts)])
        (hash-set all pkg ht)))
    (call-with-output-file*
     (build-path built-catalog-dir "pkgs-all")
     #:exists 'truncate/replace
     (lambda (o)
       (write all o)
       (newline o)))
    (call-with-output-file*
     (build-path built-catalog-dir "pkgs")
     #:exists 'truncate/replace
     (lambda (o)
       (write (hash-keys all) o)
       (newline o))))

  (delete-directory/files built-catalog-dir #:must-exist? #f)
  (make-directory* built-catalog-dir)
  (make-directory* (build-path built-catalog-dir "pkg"))
  (call-with-output-file* 
   (build-path built-catalog-dir "pkgs-all")
   (lambda (o) (displayln "#hash()" o)))
  (call-with-output-file* 
   (build-path built-catalog-dir "pkgs")
   (lambda (o) (displayln "()" o)))
  (update-built-catalog (set->list (set-subtract
                                    (set-subtract try-pkgs need-pkgs)
                                    failed-pkgs)))

  ;; ----------------------------------------
  (make-directory* (build-path built-dir "adds"))
  (make-directory* fail-dir)
  (make-directory* success-dir)
  (make-directory* install-success-dir)
  (make-directory* deps-fail-dir)

  (make-directory* dumpster-pkgs-dir)
  (make-directory* dumpster-adds-dir)

  (define (pkg-adds-file pkg)
    (build-path built-dir "adds" (format "~a-adds.rktd" pkg)))

  (define (complain failure-dest fmt . args)
    (when failure-dest
      (call-with-output-file*
       failure-dest
       #:exists 'truncate/replace
       (lambda (o) (apply fprintf o fmt args))))
    (apply eprintf fmt args)
    #f)

  ;; Build one package or a group of packages:
  (define (build-pkgs vm pkgs)
    (define flat-pkgs (flatten pkgs))
    ;; one-pkg can be a list in the case of mutual dependencies:
    (define one-pkg (and (= 1 (length pkgs)) (car pkgs)))
    (define pkgs-str (apply ~a #:separator " " flat-pkgs))

    (status (~a (make-string 40 #\=) "\n"))
    (if one-pkg
        (if (pair? one-pkg)
            (begin
              (status "Building mutually dependent packages:\n")
              (show-list one-pkg))
            (status "Building ~a\n" one-pkg))
        (begin
          (status "Building packages together:\n")
          (show-list pkgs)))

    (define failure-dest (and one-pkg
                              (pkg-failure-dest (car flat-pkgs))))
    (define install-success-dest (build-path install-success-dir
                                             (txt (car flat-pkgs))))

    (define (pkg-deps-failure-dest pkg)
      (build-path deps-fail-dir (txt pkg)))
    (define deps-failure-dest (and one-pkg
                                   (pkg-deps-failure-dest (car flat-pkgs))))

    (define (save-checksum pkg)
      (call-with-output-file*
       (build-path built-pkgs-dir (~a pkg ".orig-CHECKSUM"))
       #:exists 'truncate/replace
       (lambda (o) (write-string (pkg-checksum pkg) o))))

    (define there-dir (vm-dir vm))

    (for ([pkg (in-list flat-pkgs)])
      (define f (build-path install-success-dir (txt pkg)))
      (when (file-exists? f) (delete-file f)))

    (restore-vbox-snapshot (vm-name vm) (vm-installed-snapshot vm))
    (dynamic-wind
     (lambda () (start-vbox-vm (vm-name vm) #:max-vms (length vms)))
     (lambda ()
       (define rt (vm-remote vm))
       (make-sure-remote-is-ready rt)
       (define ok?
         (and
          ;; Try to install:
          (ssh #:show-time? #t
               rt (cd-racket vm)
               " && bin/raco pkg install -u --auto"
               (if one-pkg "" " --fail-fast")
               " " pkgs-str
               #:mode 'result
               #:failure-log failure-dest
               #:success-log install-success-dest)
          ;; Copy success log for other packages in the group:
          (for ([pkg (in-list (cdr flat-pkgs))])
            (copy-file install-success-dest
                       (build-path install-success-dir (txt pkg))
                       #t))
          (let ()
            ;; Make sure that any extra installed packages used were previously
            ;; built, since we want built packages to be consistent with a binary
            ;; installation.
            (ssh #:show-time? #t
                 rt (cd-racket vm)
                 " && bin/racket ../pkg-list.rkt --user > ../user-list.rktd")
            (scp rt (at-vm vm (~a there-dir "/user-list.rktd"))
                 (build-path work-dir "user-list.rktd"))
            (define new-pkgs (call-with-input-file*
                              (build-path work-dir "user-list.rktd")
                              read))
            (for/and ([pkg (in-list new-pkgs)])
              (or (member pkg flat-pkgs)
                  (set-member? installed-pkgs pkg)
                  (file-exists? (build-path built-catalog-dir "pkg" pkg))
                  (complain failure-dest
                            (~a "use of package not previously built: ~s;\n"
                                " maybe a dependency is missing, or maybe the package\n"
                                " failed to build on its own\n")
                            pkg))))))
       (define deps-ok?
         (and ok?
              (ssh #:show-time? #t
                   rt (cd-racket vm)
                   " && bin/raco setup -nxiID --check-pkg-deps --pkgs "
                   " " pkgs-str
                   #:mode 'result
                   #:failure-log deps-failure-dest)))
       (when (and ok? one-pkg (not deps-ok?))
         ;; Copy dependency-failure log for other packages in the group:
         (for ([pkg (in-list (cdr flat-pkgs))])
           (copy-file install-success-dest
                      (pkg-deps-failure-dest pkg)
                      #t)))
       (define doc-ok?
         (and
          ;; If we're building a single package (or set of mutually
          ;; dependent packages), then try to save generated documentation
          ;; even on failure. We'll put it in the "dumpster".
          (or ok? one-pkg)
          (ssh rt (cd-racket vm)
               " && bin/racket ../pkg-adds.rkt " pkgs-str
               " > ../pkg-adds.rktd"
               #:mode 'result
               #:failure-log (and ok? failure-dest))
          (for/and ([pkg (in-list flat-pkgs)])
            (ssh rt (cd-racket vm)
                 " && bin/raco pkg create --from-install --built"
                 " --dest " there-dir "/built"
                 " " pkg
                 #:mode 'result
                 #:failure-log (and ok? failure-dest)))))
       (cond
        [(and ok? doc-ok? (or deps-ok? one-pkg))
         (for ([pkg (in-list flat-pkgs)])
           (when (file-exists? (pkg-failure-dest pkg))
             (delete-file (pkg-failure-dest pkg)))
           (when (and deps-ok? (file-exists? (pkg-deps-failure-dest pkg)))
             (delete-file (pkg-deps-failure-dest pkg)))
           (scp rt (at-vm vm (~a there-dir "/built/" pkg ".zip"))
                built-pkgs-dir)
           (scp rt (at-vm vm (~a there-dir "/built/" pkg ".zip.CHECKSUM"))
                built-pkgs-dir)
           (scp rt (at-vm vm (~a there-dir "/pkg-adds.rktd"))
                (build-path built-dir "adds" (format "~a-adds.rktd" pkg)))
           (define deps-msg (if deps-ok? "" ", but problems with dependency declarations"))
           (call-with-output-file*
            (build-path success-dir (txt pkg))
            #:exists 'truncate/replace
            (lambda (o)
              (if one-pkg
                  (fprintf o "success~a\n" deps-msg)
                  (fprintf o "success with ~s~a\n" pkgs deps-msg))))
           (save-checksum pkg))
         (update-built-catalog flat-pkgs)]
        [else
         (when one-pkg
           ;; Record failure (for all docs in a mutually dependent set):
           (for ([pkg (in-list flat-pkgs)])
             (when (list? one-pkg)
               (unless (equal? pkg (car one-pkg))
                 (copy-file failure-dest (pkg-failure-dest (car one-pkg)) #t)))
             (save-checksum pkg))
           ;; Keep any docs that might have been built:
           (for ([pkg (in-list flat-pkgs)])
             (scp rt (at-vm vm (~a there-dir "/built/" pkg ".zip"))
                  dumpster-pkgs-dir
                  #:mode 'result)
             (scp rt (at-vm vm (~a there-dir "/built/" pkg ".zip.CHECKSUM"))
                  dumpster-pkgs-dir
                  #:mode 'result)
             (scp rt (at-vm vm (~a there-dir "/pkg-adds.rktd"))
                  (build-path dumpster-adds-dir (format "~a-adds.rktd" pkg))
                  #:mode 'result)))
         (substatus "*** failed ***\n")])
       ok?)
     (lambda ()
       (stop-vbox-vm (vm-name vm) #:save-state? #f))))

  ;; Build a group of packages, recurring on smaller groups
  ;; if the big group fails:
  (define (build-pkg-set vm pkgs)
    (define len (length pkgs))
    (define ok? (and (len . <= . max-build-together)
                     (build-pkgs vm pkgs)))
    (flush-chunk-output)
    (unless (or ok? (= 1 len))
      (define part (min (quotient len 2)
                        max-build-together))
      (build-pkg-set vm (take pkgs part))
      (build-pkg-set vm (drop pkgs part))))

  ;; Look for n packages whose dependencies are ready:
  (define (select-n n pkgs pending-pkgs)
    (cond
     [(zero? n) null]
     [(null? pkgs) null]
     [else
      (define pkg (car pkgs)) ; `pkg` can be a list of strings
      ;; Check for dependencies in `pending-pkgs`, but
      ;; we don't have to check dependencies transtively,
      ;; because the ordering of `pkgs` takes care of that.
      (cond
       [(ormap (lambda (dep) (set-member? pending-pkgs dep))
               (if (string? pkg)
                   (pkg-deps pkg)
                   (apply append (map pkg-deps pkg))))
        (select-n n (cdr pkgs) pending-pkgs)]
       [else
        (cons pkg
              (select-n (sub1 n) (cdr pkgs) pending-pkgs))])]))

  ;; try-pkgs has the same order as `pkgs`:
  (define (remove-ordered try-pkgs pkgs)
    (cond
     [(null? try-pkgs) pkgs]
     [(equal? (car try-pkgs) (car pkgs))
      (remove-ordered (cdr try-pkgs) (cdr pkgs))]
     [else
      (cons (car pkgs) (remove-ordered try-pkgs (cdr pkgs)))]))

  (struct running (vm pkgs th done?-box)
    #:property prop:evt (lambda (r)
                          (wrap-evt (running-th r)
                                    (lambda (v) r))))
  (define (start-running vm pkgs)
    (define done?-box (box #f))
    (define t (thread/chunk-output
               (lambda ()
                 (break-enabled #t)
                 (status "Sending to ~a:\n" (vm-name vm))
                 (show-list pkgs)
                 (flush-chunk-output)
                 (build-pkg-set vm pkgs)
                 (set-box! done?-box #t))))
    (running vm pkgs t done?-box))

  (define (break-running r)
    (break-thread (running-th r))
    (sync (running-th r)))

  ;; Build a group of packages, trying smaller
  ;; groups if the whole group fails or is too
  ;; big:
  (define (build-all-pkgs pkgs)
    ;; pkgs is a list of string and lists (for mutual dependency)
    (let loop ([pkgs pkgs]
               [pending-pkgs (list->set pkgs)]
               [vms vms]
               [runnings null]
               [error? #f])
      (define (wait)
        (define r
          (with-handlers ([exn:break? (lambda (exn)
                                        (log-error "breaking...")
                                        (for-each break-running runnings)
                                        (wait-chunk-output)
                                        (raise exn))])
            (parameterize-break
             #t
             (apply sync runnings))))
        (loop pkgs
              (set-subtract pending-pkgs (list->set (running-pkgs r)))
              (cons (running-vm r) vms)
              (remq r runnings)
              (or error? (not (unbox (running-done?-box r))))))
      (cond
       [error?
        (if (null? runnings)
            (error "a build task ended prematurely")
            (wait))]
       [(and (null? pkgs)
             (null? runnings))
        ;; Done
        (void)]
       [(null? vms)
        ;; All VMs busy; wait for one to finish
        (wait)]
       [else
        (define try-pkgs (select-n max-build-together pkgs pending-pkgs))
        (cond
         [(null? try-pkgs)
          ;; Nothing to do until a dependency finished; wait
          (wait)]
         [else
          (loop (remove-ordered try-pkgs pkgs)
                pending-pkgs
                (cdr vms)
                (cons (start-running (car vms) try-pkgs)
                      runnings)
                error?)])])))

  ;; Build all of the out-of-date packages:
  (unless skip-build?
    (if (= 1 (length vms))
        ;; Sequential builds:
        (build-pkg-set (car vms) need-pkgs-list)
        ;; Parallel builds:
        (parameterize-break
         #f
         (build-all-pkgs need-pkgs-list))))

  ;; ----------------------------------------
  (status "Assembling documentation\n")

  (define available-pkgs
    (for/set ([pkg (in-list all-pkg-names)]
              #:when
              (let ()
                (define checksum (pkg-checksum pkg))
                (define checksum-file (pkg-checksum-file pkg))
                (and (file-exists? checksum-file)
                     (file-exists? (pkg-zip-file pkg))
                     (file-exists? (pkg-zip-checksum-file pkg)))))
      pkg))

  (define adds-pkgs
    (for/hash ([pkg (in-set available-pkgs)])
      (define adds-file (pkg-adds-file pkg))
      (define ht (call-with-input-file* adds-file read))
      (values pkg (hash-ref ht pkg null))))
  
  (define doc-pkgs
    (for/set ([(k l) (in-hash adds-pkgs)]
              #:when (for/or ([v (in-list l)])
                       (eq? (car v) 'doc)))
      k))

  (define doc-pkg-list
    (sort (set->list doc-pkgs) string<?))

  (define install-adds-pkgs
    (call-with-input-file*
     (build-path work-dir "install-adds.rktd")
     read))
  (define install-doc-pkgs
    (for/set ([(k l) (in-hash install-adds-pkgs)]
              #:when (for/or ([v (in-list l)])
                       (eq? (car v) 'doc)))
      k))

  (substatus "Packages with documentation:\n")
  (show-list doc-pkg-list)
  
  ;; `conflict-pkgs` have a direct conflict, while `no-conflict-pkgs`
  ;; have no direct conflict and no dependency with a conflict
  (define-values (conflict-pkgs no-conflict-pkgs)
    (let ()
      (define (add-providers ht pkgs)
        (for*/fold ([ht ht]) ([(k v) (in-hash pkgs)]
                              [(d) (in-list v)])
          (hash-update ht d (lambda (l) (set-add l k)) (set))))
      (define providers (add-providers (add-providers (hash) adds-pkgs)
                                       (call-with-input-file*
                                        (build-path work-dir "install-adds.rktd")
                                        read)))
      (define conflicts
        (for/list ([(k v) (in-hash providers)]
                   #:when ((set-count v) . > . 1))
          (cons k v)))
      (cond
       [(null? conflicts)
        (values (set) available-pkgs)]
       [else
        (define (show-conflicts)
          (substatus "Install conflicts:\n")
          (for ([v (in-list conflicts)])
            (substatus " ~a ~s:\n" (caar v) (cdar v))
            (show-list #:indent " " (sort (set->list (cdr  v)) string<?))))
        (show-conflicts)
        (with-output-to-file "conflicts.txt"
          #:exists 'truncate/replace
          show-conflicts)
        (define conflicting-pkgs
          (for/fold ([s (set)]) ([v (in-list conflicts)])
            (set-union s (cdr v))))
        (define reverse-deps
          (for*/fold ([ht (hash)]) ([pkg (in-set available-pkgs)]
                                    [dep (in-list (pkg-deps pkg))])
            (hash-update ht dep (lambda (s) (set-add s pkg)) (set))))
        (define disallowed-pkgs
          (let loop ([pkgs conflicting-pkgs] [conflicting-pkgs conflicting-pkgs])
            (define new-pkgs (for*/set ([p (in-set conflicting-pkgs)]
                                        [rev-dep (in-set (hash-ref reverse-deps p (set)))]
                                        #:unless (set-member? pkgs rev-dep))
                               rev-dep))
            (if (set-empty? new-pkgs)
                pkgs
                (loop (set-union pkgs new-pkgs) new-pkgs))))
        (substatus "Packages disallowed due to conflicts:\n")
        (show-list (sort (set->list disallowed-pkgs) string<?))
        (values conflicting-pkgs
                (set-subtract available-pkgs disallowed-pkgs))])))

  (define no-conflict-doc-pkgs (set-intersect (list->set doc-pkg-list) no-conflict-pkgs))
  (define no-conflict-doc-pkg-list (sort (set->list no-conflict-doc-pkgs) string<?))

  (unless skip-docs?
    ;; Save "doc" as "prev-doc", so we can preserve any documentation
    ;; that successfully built in the past. If "prev-doc" exists,
    ;; assume that a previous "doc" run didn't complete, so keep referring
    ;; to the old "prev-doc".
    (define prev-doc-dir (build-path work-dir "prev-doc"))
    (when (and (directory-exists? doc-dir)
               (not (directory-exists? prev-doc-dir)))
      (rename-file-or-directory doc-dir prev-doc-dir))

    (define prev-docs
      (if (directory-exists? prev-doc-dir)
          (for/fold ([ht (hash)]) ([d (in-list (directory-list prev-doc-dir))])
            (define m (regexp-match #rx"^[^@]+@([^@]+)$" d))
            (if m
                (hash-update ht (cadr m) (lambda (l) (cons d l)) null)
                ht))
          (hash)))

    (define vm (car vms))
    (restore-vbox-snapshot (vm-name vm) (vm-installed-snapshot vm))
    (define rt (vm-remote vm))

    ;; Get fully installed docs for non-conflicting packages:
    (dynamic-wind
     (lambda () (start-vbox-vm (vm-name vm)))
     (lambda ()
       (define rt (vm-remote vm))
       (make-sure-remote-is-ready rt)
       (ssh #:show-time? #t
            rt (cd-racket vm)
            " && bin/raco pkg install -i --auto"
            " " (apply ~a #:separator " " no-conflict-doc-pkg-list))
       (ssh rt (cd-racket vm)
            " && tar zcf ../all-doc.tgz doc")
       (scp rt (at-vm vm (~a (vm-dir vm) "/all-doc.tgz"))
            (build-path work-dir "all-doc.tgz")))
     (lambda ()
       (stop-vbox-vm (vm-name vm) #:save-state? #f)))
    (untgz "all-doc.tgz")

    ;; Clear links:
    (for ([f (in-list (directory-list doc-dir #:build? #t))])
      (when (regexp-match? #rx"@" f)
        (delete-directory/files f)))

    ;; For completeness, add links for installer's docs:
    (for ([pkg (in-set install-doc-pkgs)])
      (for ([a (in-list (hash-ref install-adds-pkgs pkg))]
            #:when (eq? 'doc (car a)))
        (define doc (cdr a))
        (make-file-or-directory-link doc (build-path doc-dir (~a doc "@" pkg)))))

    ;; Add documentation for conflicting packages, and add links for
    ;; each package:
    (for ([pkg (in-set doc-pkgs)])
      (define docs (for/list ([a (in-list (hash-ref adds-pkgs pkg))]
                              #:when (eq? 'doc (car a)))
                     (cdr a)))
      (cond
       [(set-member? no-conflict-doc-pkgs pkg)
        ;; Create a link for fully installed documentation:
        (for ([doc (in-list docs)])
          (make-file-or-directory-link doc (build-path doc-dir (~a doc "@" pkg))))]
       [else
        ;; Extract successfully built but not fully installed documentation:
        (substatus "Trying to extract ~s docs\n" pkg)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (eprintf "~a\n" (exn-message exn)))])
          (extract-documentation (pkg-zip-file pkg) pkg doc-dir))]))

    ;; Add salvageable docs from the dumpster, and fall back as a last resort
    ;; to documention in "prev-doc":
    (for ([pkg (in-set try-pkgs)])
      (unless (set-member? available-pkgs pkg)
        (define adds-file (build-path dumpster-adds-dir (format "~a-adds.rktd" pkg)))
        (define zip-file (build-path dumpster-pkgs-dir (format "~a.zip" pkg)))
        (define adds* (and (file-exists? adds-file)
                           (file-exists? zip-file)
                           (with-handlers ([exn:fail? (lambda (exn) #f)])
                             (call-with-input-file* adds-file read))))
        (define adds (and (hash? adds*)
                          (hash-ref adds* pkg #f)))
        (when (and (list? adds)
                   (ormap (lambda (a) (and (pair? a) (eq? (car a) 'doc)))
                          adds))
          (substatus "Trying to salvage ~s docs\n" pkg)
          (with-handlers ([exn:fail? (lambda (exn)
                                       (eprintf "~a\n" (exn-message exn)))])
            (extract-documentation zip-file pkg doc-dir))
          (for ([f (in-list (hash-ref prev-docs pkg null))])
            (unless (directory-exists? (build-path doc-dir f))
              (substatus "Salvaging previously built ~a\n" f)
              (copy-directory/files (build-path prev-doc-dir f)
                                    (build-path doc-dir f)))))))

    ;; The "docs" directory now have everything that we want to keep from
    ;; "prev-docs". To make the delete effectively atomic, move and then
    ;; delete.
    (when (directory-exists? prev-doc-dir)
      (define old-prev-doc-dir (build-path work-dir "old-prev-doc"))
      (when (directory-exists? old-prev-doc-dir)
        (delete-directory/files old-prev-doc-dir))
      (rename-file-or-directory prev-doc-dir old-prev-doc-dir)
      (delete-directory/files old-prev-doc-dir)))
  
  ;; ----------------------------------------

  (unless skip-summary?
    (define (path->relative p)
      (define work (explode-path work-dir))
      (define dest (explode-path p))
      (unless (equal? work (take dest (length work)))
        (error "not relative"))
      (string-join (map path->string (drop dest (length work))) "/"))
    
    (define summary-ht
      (for/hash ([pkg (in-set (set-subtract try-pkgs
                                            (list->set summary-omit-pkgs)))])
        (define failed? (file-exists? (pkg-failure-dest pkg)))
        (define succeeded? (file-exists? (build-path install-success-dir (txt pkg))))
        (define status
          (cond
           [(and failed? (not succeeded?)) 'failure]
           [(and succeeded? (not failed?)) 'success]
           [(and succeeded? failed?) 'confusion]
           [else 'unknown]))
        (define dep-status
          (if (eq? status 'success)
              (if (file-exists? (build-path deps-fail-dir (txt pkg)))
                  'failure
                  'success)
              'unknown))
        (define adds (let ([adds-file (if (eq? status 'success)
                                          (pkg-adds-file pkg)
                                          (build-path dumpster-adds-dir (format "~a-adds.rktd" pkg)))])
                       (if (file-exists? adds-file)
                           (hash-ref (call-with-input-file* adds-file read)
                                     pkg
                                     null)
                           null)))
        (define conflicts? (and (eq? status 'success)
                                (not (set-member? no-conflict-pkgs pkg))))
        (define docs (for/list ([add (in-list adds)]
                                #:when (eq? (car add) 'doc))
                       (cdr add)))
        (values
         pkg
         (hash 'success-log (and (or (eq? status 'success)
                                     (eq? status 'confusion))
                                 (path->relative (build-path install-success-dir (txt pkg))))
               'failure-log (and (or (eq? status 'failure)
                                     (eq? status 'confusion))
                                 (path->relative (pkg-failure-dest pkg)))
               'dep-failure-log (and (eq? dep-status 'failure)
                                     (path->relative (build-path deps-fail-dir (txt pkg))))
               'docs (for/list ([doc (in-list docs)])
                       (define path (~a "doc/" (~a doc "@" pkg) "/index.html"))
                       (if (or (not (eq? status 'success))
                               conflicts?)
                           (if (directory-exists? (build-path doc-dir (~a doc "@" pkg)))
                               (if (set-member? available-pkgs pkg)
                                   (doc/extract doc path)
                                   (doc/salvage doc path))
                               (doc/none doc))
                           (doc/main doc path)))
               'author (pkg-author pkg)
               'conflicts-log (and conflicts?
                                   (if (set-member? conflict-pkgs pkg)
                                       "conflicts.txt"
                                       (conflicts/indirect "conflicts.txt")))))))

    ;; Add info for docs in the installer:
    (define full-summary-ht
      (for/fold ([ht summary-ht]) ([pkg (in-set install-doc-pkgs)])
        (define docs (for/list ([a (in-list (hash-ref install-adds-pkgs pkg))]
                                #:when (eq? 'doc (car a)))
                       (define doc (cdr a))
                       (define path (~a "doc/" (~a doc "@" pkg) "/index.html"))
                       (doc/main doc path)))
        (hash-set ht pkg (hash 'docs docs))))

    (call-with-output-file*
     (build-path work-dir "summary.rktd")
     #:exists 'truncate/replace
     (lambda (o)
       (write full-summary-ht o)
       (newline o)))

    (summary-page summary-ht work-dir))

  ;; ----------------------------------------

  (unless skip-site?
    (define site-file (build-path work-dir "site.tgz"))
    (status "Packing site to ~a\n" site-file)

    (define (wpath . a) (apply build-path work-dir a))
    (define skip-paths (set (wpath "installer")
                            (wpath "server" "archive")
                            (wpath "server" "built" "catalog")
                            (wpath "server" "built" "pkgs")
                            (wpath "server" "built" "adds")
                            (wpath "dumpster")
                            (wpath "table.rktd")
                            (wpath "state.sqlite")
                            (wpath "all-doc.tgz")
                            (wpath "install-doc.tgz")
                            (wpath "install-adds.rktd")
                            (wpath "user-list.rktd")
                            (wpath "prev-doc")
                            (wpath "old-prev-doc")
                            (wpath "doc" "docindex.sqlite")
                            (wpath "site.tgz")))
    (parameterize ([current-directory work-dir])
      (define files (for/list ([f (in-directory #f (lambda (p)
                                                     (not (set-member? skip-paths p))))]
                               #:unless (set-member? skip-paths (path->complete-path f)))
                      f))
      (call-with-output-file*
       site-file
       #:exists 'truncate/replace
       (lambda (o)
         (define-values (i2 o2) (make-pipe 40960))
         (thread (lambda ()
                   (dynamic-wind
                    void
                    (lambda () (tar->output files o2))
                    (lambda () (close-output-port o2)))))
         (gzip-through-ports i2 o #f (current-seconds))))))

  ;; ----------------------------------------
  
  (void))
