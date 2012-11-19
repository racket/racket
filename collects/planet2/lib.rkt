#lang racket/base
(require net/url
         json
         openssl/sha1
         racket/contract
         racket/match
         racket/system
         racket/path
         racket/file
         setup/link
         setup/pack
         setup/unpack
         setup/dirs
         racket/port
         racket/list
         racket/function
         racket/dict
         racket/set
         unstable/debug
         racket/string
         file/untgz
         "util.rkt"
         "util-plt.rkt")

(define current-install-system-wide?
  (make-parameter #f))

(define (file->value* pth def)
  (with-handlers ([exn:fail? (λ (x) def)])
    (file->value pth)))

(define (path->bytes* pkg)
  (cond
    [(path? pkg)
     (path->bytes pkg)]
    [(string? pkg)
     (path->bytes (string->path pkg))]
    [(bytes? pkg)
     pkg]))

(define (directory-path-no-slash pkg)
  (bytes->path (regexp-replace* #rx#"/$" (path->bytes* pkg) #"")))

(define (absolute-collects-dir)
  (path->complete-path
   (find-system-path 'collects-dir)
   (path-only (find-executable-path (find-system-path 'exec-file)))))

(define (directory-list* d)
  (append-map
   (λ (pp)
     (define p (build-path d pp))
     (if (directory-exists? p)
       (map (curry build-path pp)
            (directory-list* p))
       (list pp)))
   (directory-list d)))

(define (simple-form-path* p)
  (path->string (simple-form-path p)))

(define (untar pkg pkg-dir #:strip-components [strip-components 0])
  (make-directory* pkg-dir)
  (untgz pkg #:dest pkg-dir #:strip-count strip-components))

(define (download-file! url file #:fail-okay? [fail-okay? #f])
  (with-handlers
      ([exn:fail?
        (λ (x)
          (unless fail-okay?
            (raise x)))])
    (make-parent-directory* file)
    (dprintf "\t\tDownloading ~a to ~a\n" (url->string url) file)
    (call-with-output-file file
      (λ (op)
        (call/input-url+200
         url
         (λ (ip) (copy-port ip op)))))))

(define (pkg-dir)
  (build-path (if (current-install-system-wide?)
                (find-lib-dir)
                (find-system-path 'addon-dir))
              "pkgs"))
(define (pkg-config-file)
  (build-path (pkg-dir) "config.rktd"))
(define (pkg-db-file)
  (build-path (pkg-dir) "pkgs.rktd"))
(define (pkg-installed-dir)
  (build-path (pkg-dir) "installed"))
(define (pkg-lock-file)
  (make-lock-file-name (pkg-db-file)))

(for-each make-directory*
          (list (pkg-dir) (pkg-installed-dir)))

(define (with-package-lock* t)
  (make-directory* (pkg-dir))
  (call-with-file-lock/timeout
   #f 'exclusive
   t
   (λ () (error 'planet2 "Could not acquire package lock: ~e"
                (pkg-lock-file)))
   #:lock-file (pkg-lock-file)))
(define-syntax-rule (with-package-lock e ...)
  (with-package-lock* (λ () e ...)))

(define (read-pkg-cfg/def k)
  (define c (read-pkg-cfg))
  (hash-ref c k
            (λ ()
              (match k
                ["indexes"
                 (list "https://plt-etc.byu.edu:9004"
                       "https://plt-etc.byu.edu:9003")]))))

(define (package-index-lookup pkg)
  (or
   (for/or ([i (in-list (read-pkg-cfg/def "indexes"))])
     (call/input-url+200
      (combine-url/relative
       (string->url i)
       (format "/pkg/~a" pkg))
      read))
   (error 'planet2 "Cannot find package ~a on indexes" pkg)))

(define (remote-package-checksum pkg)
  (match pkg
    [`(pns ,pkg-name)
     (hash-ref (package-index-lookup pkg-name) 'checksum)]
    [`(url ,pkg-url-str)
     (package-url->checksum pkg-url-str)]))

(define (read-file-hash file)
  (define the-db
    (with-handlers ([exn? (λ (x) (hash))])
      (file->value file)))
  the-db)
(define (write-file-hash! file new-db)
  (make-parent-directory* file)
  (with-output-to-file file
    #:exists 'replace
    (λ () (write new-db))))

(define (read-pkg-db)
  (read-file-hash (pkg-db-file)))

(define (package-info pkg-name [fail? #t])
  (define db (read-pkg-db))
  (define pi (hash-ref db pkg-name #f))
  (cond
    [pi
     pi]
    [(not fail?)
     #f]
    [else
     (error 'planet2 "Package ~e not currently installed; ~e are installed"
            pkg-name
            (hash-keys db))]))

(define (update-pkg-db! pkg-name info)
  (write-file-hash!
   (pkg-db-file)
   (hash-set (read-pkg-db) pkg-name info)))
(define (remove-from-pkg-db! pkg-name)
  (write-file-hash!
   (pkg-db-file)
   (hash-remove (read-pkg-db) pkg-name)))
(define (read-pkg-cfg)
  (read-file-hash (pkg-config-file)))
(define (update-pkg-cfg! key val)
  (write-file-hash!
   (pkg-config-file)
   (hash-set (read-pkg-cfg) key val)))

(struct pkg-info (orig-pkg checksum auto?) #:prefab)
(struct install-info (name orig-pkg directory clean? checksum))

(define (update-install-info-orig-pkg if op)
  (struct-copy install-info if
               [orig-pkg op]))
(define (update-install-info-checksum if op)
  (struct-copy install-info if
               [checksum op]))



(define (package-directory pkg-name)
  (match-define (pkg-info orig-pkg checksum _)
                (package-info pkg-name))
  (match orig-pkg
    [`(link ,orig-pkg-dir)
     orig-pkg-dir]
    [_
     (build-path (pkg-installed-dir) pkg-name)]))

(define (remove-package pkg-name)
  (match-define (pkg-info orig-pkg checksum _)
                (package-info pkg-name))
  (define pkg-dir (package-directory pkg-name))
  (remove-from-pkg-db! pkg-name)
  (match orig-pkg
    [`(link ,_)
     (links pkg-dir
            #:remove? #t
            #:user? (not (current-install-system-wide?))
            #:root? #t)]
    [_
     (links pkg-dir
            #:remove? #t
            #:user? (not (current-install-system-wide?))
            #:root? #t)
     (delete-directory/files pkg-dir)]))

(define (remove-packages in-pkgs
                         #:force? [force? #f]
                         #:auto? [auto? #f])
  (define db (read-pkg-db))
  (define all-pkgs
    (hash-keys db))
  (define all-pkgs-set
    (list->set all-pkgs))
  (define pkgs
    (if auto?
      (set->list
       (set-subtract
        (list->set
         (filter
          (λ (p) (pkg-info-auto? (hash-ref db p)))
          all-pkgs))
        (list->set
         (append-map package-dependencies
                     all-pkgs))))
      in-pkgs))
  (unless force?
    (define pkgs-set (list->set pkgs))
    (define remaining-pkg-db-set
      (set-subtract all-pkgs-set
                    pkgs-set))
    (define deps-to-be-removed
      (set-intersect
       pkgs-set
       (list->set
        (append-map package-dependencies
                    (set->list
                     remaining-pkg-db-set)))))
    (unless (set-empty? deps-to-be-removed)
      (error 'planet2 "Cannot remove packages that are dependencies of other packages: ~e"
             (set->list deps-to-be-removed))))
  (for-each remove-package pkgs))

(define (install-packages
         #:old-infos [old-infos empty]
         #:old-auto+pkgs [old-auto+pkgs empty]
         #:pre-succeed [pre-succeed void]
         #:dep-behavior [dep-behavior #f]
         #:updating? [updating? #f]
         #:ignore-checksums? [ignore-checksums? #f]
         #:link? [link? #f]
         #:force? [force? #f]
         auto+pkgs)
  (define check-sums? (not ignore-checksums?))
  (define (install-package pkg
                           #:pkg-name [given-pkg-name #f])
    (define pkg-url (and (string? pkg) (string->url pkg)))
    (cond
      [(file-exists? pkg)
       (define checksum-pth (format "~a.CHECKSUM" pkg))
       (define expected-checksum
         (and (file-exists? checksum-pth)
              check-sums?
              (file->string checksum-pth)))
       (define actual-checksum
         (with-input-from-file pkg
           (λ ()
             (sha1 (current-input-port)))))
       (unless (or (not expected-checksum)
                   (string=? expected-checksum actual-checksum))
         (error 'pkg "Incorrect checksum on package: expected ~e, got ~e"
                expected-checksum actual-checksum))
       (define checksum
         actual-checksum)
       (define pkg-format (filename-extension pkg))
       (define pkg-name
         (or given-pkg-name
             (regexp-replace
              (regexp
               (format "~a$" (regexp-quote (format ".~a" pkg-format))))
              (path->string (file-name-from-path pkg))
              "")))
       (define pkg-dir
         (make-temporary-file (string-append "~a-" pkg-name)
                              'directory))
       (dynamic-wind
           void
           (λ ()
             (make-directory* pkg-dir)

             (match pkg-format
               [#"tgz"
                (untar pkg pkg-dir)]
               [#"zip"
                (system* (find-executable-path "unzip")
                         "-n" pkg "-d" pkg-dir)]
               [#"plt"
                (unplt pkg pkg-dir)]
               [x
                (error 'pkg "Invalid package format: ~e" x)])

             (update-install-info-checksum
              (update-install-info-orig-pkg
               (install-package pkg-dir
                                #:pkg-name pkg-name)
               `(file ,(simple-form-path* pkg)))
              checksum))
           (λ ()
             (delete-directory/files pkg-dir)))]
      [(directory-exists? pkg)
       (let ([pkg (directory-path-no-slash pkg)])
         (define pkg-name
           (or given-pkg-name (path->string (file-name-from-path pkg))))
         (cond
           [link?
            (install-info pkg-name
                          `(link ,(simple-form-path* pkg))
                          pkg
                          #f #f)]
           [else
            (define pkg-dir
              (make-temporary-file "pkg~a" 'directory))
            (delete-directory pkg-dir)
            (make-parent-directory* pkg-dir)
            (copy-directory/files pkg pkg-dir)
            (install-info pkg-name
                          `(dir ,(simple-form-path* pkg))
                          pkg-dir
                          #t #f)]))]
      [(url-scheme pkg-url)
       =>
       (lambda (scheme)
         (define orig-pkg `(url ,pkg))
         (define checksum (remote-package-checksum orig-pkg))
         (define info
           (update-install-info-orig-pkg
            (match scheme
              ["github"
               (match-define (list* user repo branch path)
                             (map path/param-path (url-path/no-slash pkg-url)))
               (define new-url
                 (url "https" #f "github.com" #f #t
                      (map (λ (x) (path/param x empty))
                           (list user repo "tarball" branch))
                      empty
                      #f))
               (define tmp.tgz
                 (make-temporary-file
                  (string-append
                   "~a-"
                   (format "~a.~a.tgz" repo branch))
                  #f))
               (delete-file tmp.tgz)
               (define tmp-dir
                 (make-temporary-file
                  (string-append
                   "~a-"
                   (format "~a.~a" repo branch))
                  'directory))
               (define package-path
                 (apply build-path tmp-dir path))

               (dynamic-wind
                   void
                   (λ ()
                     (download-file! new-url tmp.tgz)
                     (dynamic-wind
                         void
                         (λ ()
                           (untar tmp.tgz tmp-dir #:strip-components 1)
                           (install-package (path->string package-path)
                                            #:pkg-name given-pkg-name))
                         (λ ()
                           (delete-directory/files tmp-dir))))
                   (λ ()
                     (delete-directory/files tmp.tgz)))]
              [_
               (define url-last-component
                 (path/param-path (last (url-path pkg-url))))
               (define url-looks-like-directory?
                 (string=? "" url-last-component))
               (define-values
                 (package-path package-name download-package!)
                 (cond
                   [url-looks-like-directory?
                    (define package-name
                      (path/param-path
                       (second (reverse (url-path pkg-url)))))
                    (define package-path
                      (make-temporary-file
                       (string-append
                        "~a-"
                        package-name)
                       'directory))
                    (define (path-like f)
                      (build-path package-path f))
                    (define (url-like f)
                      (combine-url/relative pkg-url f))
                    (values package-path
                            package-name
                            (λ ()
                              (printf "\tCloning remote directory\n")
                              (make-directory* package-path)
                              (define manifest
                                (call/input-url+200
                                 (url-like "MANIFEST")
                                 port->lines))
                              (for ([f (in-list manifest)])
                                (download-file! (url-like f)
                                                (path-like f)))))]
                   [else
                    (define package-path
                      (make-temporary-file
                       (string-append
                        "~a-"
                        url-last-component)
                       #f))
                    (delete-file package-path)
                    (values package-path
                            (regexp-replace
                             #rx"\\.[^.]+$"
                             url-last-component
                             "")
                            (λ ()
                              (dprintf "\tAssuming URL names a file\n")
                              (download-file! pkg-url package-path)))]))
               (dynamic-wind
                   void
                   (λ ()
                     (download-package!)
                     (define pkg-name
                       (or given-pkg-name
                           package-name))
                     (dprintf "\tDownloading done, installing ~a as ~a\n"
                              package-path pkg-name)
                     (install-package package-path
                                      #:pkg-name
                                      pkg-name))
                   (λ ()
                     (when (or (file-exists? package-path)
                               (directory-exists? package-path))
                       (delete-directory/files package-path))))])
            orig-pkg))
         (when (and check-sums?
                    (install-info-checksum info)
                    (not checksum))
           (error 'planet2 "Remote package ~a had no checksum"
                  pkg))
         (when (and checksum
                    (install-info-checksum info)
                    check-sums?
                    (not (equal? (install-info-checksum info) checksum)))
           (error 'planet2 "Incorrect checksum on package ~e: expected ~e, got ~e"
                  pkg
                  (install-info-checksum info) checksum))
         (update-install-info-checksum
          info
          checksum))]
      [else
       (define index-info (package-index-lookup pkg))
       (define source (hash-ref index-info 'source))
       (define checksum (hash-ref index-info 'checksum))
       (define info (install-package source
                                     #:pkg-name (or given-pkg-name pkg)))
       (when (and (install-info-checksum info)
                  check-sums?
                  (not (equal? (install-info-checksum info) checksum)))
         (error 'planet2 "Incorrect checksum on package: ~e" pkg))
       (update-install-info-orig-pkg
        (update-install-info-checksum
         info
         checksum)
        `(pns ,pkg))]))
  (define db (read-pkg-db))
  (define (install-package/outer infos auto+pkg info)
    (match-define (cons auto? pkg)
                  auto+pkg)
    (match-define
     (install-info pkg-name orig-pkg pkg-dir clean? checksum)
     info)
    (define pns? (eq? 'pns (first orig-pkg)))
    (define (clean!)
      (when clean?
        (delete-directory/files pkg-dir)))
    (define simultaneous-installs
      (list->set (map install-info-name infos)))
    (cond
      [(and (not updating?) (package-info pkg-name #f))
       (clean!)
       (error 'planet2 "~e is already installed" pkg-name)]
      [(and
        (not force?)
        (for/or ([f (in-list (directory-list* pkg-dir))]
                 #:when (member (filename-extension f)
                                (list #"rkt" #"ss")))
          (or
           ;; Compare with Racket
           (and (file-exists? (build-path (absolute-collects-dir) f))
                (cons "racket" f))
           ;; Compare with installed packages
           (for/or ([other-pkg (in-hash-keys db)]
                    #:unless (and updating? (equal? other-pkg pkg-name)))
             (define p (build-path (package-directory other-pkg) f))
             (and (file-exists? p)
                  (cons other-pkg f)))
           ;; Compare with simultaneous installs
           (for/or ([other-pkg-info (in-list infos)]
                    #:unless (eq? other-pkg-info info))
             (define p (build-path (install-info-directory other-pkg-info) f))
             (and (file-exists? p)
                  (cons (install-info-name other-pkg-info) f))))))
       =>
       (λ (conflicting-pkg*file)
         (clean!)
         (match-define (cons conflicting-pkg file) conflicting-pkg*file)
         (error 'planet2 "~e conflicts with ~e: ~e" pkg conflicting-pkg file))]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define meta (file->value* (build-path pkg-dir "METADATA.rktd") empty))
          (define deps (dict-ref meta 'dependency empty))
          (define unsatisfied-deps
            (filter-not (λ (dep)
                          (or (set-member? simultaneous-installs dep)
                              (hash-has-key? db dep)))
                        deps))
          (and (not (empty? unsatisfied-deps))
               unsatisfied-deps)))
       =>
       (λ (unsatisfied-deps)
         (match
             (or dep-behavior
                 (if pns?
                   'search-ask
                   'fail))
           ['fail
            (clean!)
            (error 'planet2 "missing dependencies: ~e" unsatisfied-deps)]
           ['search-auto
            (printf "The following packages are listed as dependencies, but are not currently installed, so we will automatically install them.\n")
            (printf "\t")
            (for ([p (in-list unsatisfied-deps)])
              (printf "~a " p))
            (printf "\n")
            (raise (vector infos unsatisfied-deps))]
           ['search-ask
            (printf "The following packages are listed as dependencies, but are not currently installed:\n")
            (printf "\t")
            (for ([p (in-list unsatisfied-deps)])
              (printf "~a " p))
            (printf "\n")
            (let loop ()
              (printf "Would you like to install them via your package indices? [Yn] ")
              (flush-output)
              (match (read-line)
                [(or "y" "Y" "")
                 (raise (vector infos unsatisfied-deps))]
                [(or "n" "N")
                 (clean!)
                 (error 'planet2 "missing dependencies: ~e" unsatisfied-deps)]
                [x
                 (eprintf "Invalid input: ~e\n" x)
                 (loop)]))]))]
      [else
       (λ ()
         (define final-pkg-dir
           (cond
             [clean?
              (define final-pkg-dir (build-path (pkg-installed-dir) pkg-name))
              (make-parent-directory* final-pkg-dir)
              (copy-directory/files pkg-dir final-pkg-dir)
              (clean!)
              final-pkg-dir]
             [else
              pkg-dir]))
         (dprintf "creating link to ~e" final-pkg-dir)
         (links final-pkg-dir
                #:user? (not (current-install-system-wide?))
                #:root? #t)
         (define this-pkg-info
           (pkg-info orig-pkg checksum auto?))
         (dprintf "updating db with ~e to ~e" pkg-name this-pkg-info)
         (update-pkg-db! pkg-name this-pkg-info))]))
  (define infos
    (map install-package (map cdr auto+pkgs)))
  (define do-its
    (map (curry install-package/outer (append old-infos infos))
         (append old-auto+pkgs auto+pkgs)
         (append old-infos infos)))
  (pre-succeed)
  (for-each (λ (t) (t)) do-its))

(define (install-cmd pkgs
                     #:old-infos [old-infos empty]
                     #:old-auto+pkgs [old-auto+pkgs empty]
                     #:force? [force #f]
                     #:link? [link #f]
                     #:ignore-checksums? [ignore-checksums #f]
                     #:pre-succeed [pre-succeed void]
                     #:dep-behavior [dep-behavior #f]
                     #:updating? [updating? #f])
  (with-handlers ([vector?
                   (match-lambda
                    [(vector new-infos deps)
                     (dprintf "\nInstallation failed with new deps: ~a\n\n"
                              deps)

                     (install-cmd
                      #:old-infos new-infos
                      #:old-auto+pkgs (append old-auto+pkgs pkgs)
                      #:force? force
                      #:link? link
                      #:ignore-checksums? ignore-checksums
                      #:dep-behavior dep-behavior
                      #:pre-succeed pre-succeed
                      #:updating? updating?
                      (map (curry cons #t) deps))])])
    (install-packages
     #:old-infos old-infos
     #:old-auto+pkgs old-auto+pkgs
     #:force? force
     #:link? link
     #:ignore-checksums? ignore-checksums
     #:dep-behavior dep-behavior
     #:pre-succeed pre-succeed
     #:updating? updating?
     pkgs)))

(define (update-is-possible? pkg-name)
  (match-define (pkg-info orig-pkg checksum _)
                (package-info pkg-name))
  (define ty (first orig-pkg))
  (not (member ty '(link dir file))))

(define (update-package pkg-name)
  (match-define (pkg-info orig-pkg checksum auto?)
                (package-info pkg-name))
  (match orig-pkg
    [`(link ,_)
     (error 'planet2 "Cannot update linked packages (~e is linked to ~e)"
            pkg-name
            orig-pkg)]
    [`(dir ,_)
     (error 'planet2 "Cannot update packages installed locally. (~e was installed via a local directory.)"
            pkg-name)]
    [`(file ,_)
     (error 'planet2 "Cannot update packages installed locally. (~e was installed via a local file.)"
            pkg-name)]
    [`(,_ ,orig-pkg-desc)
     (define new-checksum
       (remote-package-checksum orig-pkg))
     (and new-checksum
          (not (equal? checksum new-checksum))
          (cons pkg-name (cons auto? orig-pkg-desc)))]))

(define (package-dependencies pkg-name)
  (define pkg-dir (package-directory pkg-name))
  (define meta (file->value* (build-path pkg-dir "METADATA.rktd") empty))
  (dict-ref meta 'dependency empty))

(define (update-packages in-pkgs
                         #:all? [all? #f]
                         #:dep-behavior [dep-behavior #f]
                         #:deps? [deps? #f])
  (define pkgs
    (cond
      [(and all? (empty? in-pkgs))
       (filter update-is-possible? (hash-keys (read-pkg-db)))]
      [deps?
       (append-map
        package-dependencies
        in-pkgs)]
      [else
       in-pkgs]))
  (define to-update (filter-map update-package pkgs))
  (cond
    [(empty? to-update)
     (printf "No updates available\n")]
    [else
     (install-cmd
      #:updating? #t
      #:pre-succeed (λ () (for-each (compose remove-package car) to-update))
      #:dep-behavior dep-behavior
      (map cdr to-update))]))

(define (show-cmd)
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (table-display
     (list*
      (list "Package(auto?)" "Checksum" "Source")
      (for/list ([pkg (in-list pkgs)])
        (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
        (list (format "~a~a"
                      pkg
                      (if auto?
                        "*"
                        ""))
              (format "~a" checksum)
              (format "~a" orig-pkg)))))))

(define (config-cmd config:set key+vals)
  (cond
    [config:set
     (match key+vals
       [(list* (and key "indexes") val)
        (update-pkg-cfg! "indexes" val)]
       [(list key)
        (error 'planet2 "unsupported config key: ~e" key)]
       [(list)
        (error 'planet2 "must provide config key")])]
    [else
     (match key+vals
       [(list key)
        (match key
          ["indexes"
           (for ([s (in-list (read-pkg-cfg/def "indexes"))])
             (printf "~a\n" s))]
          [_
           (error 'planet2 "unsupported config key: ~e" key)])]
       [(list)
        (error 'planet2 "must provide config key")]
       [_
        (error 'planet2 "must provide only config key")])]))

(define (create-cmd create:format maybe-dir)
  (begin
    (define dir (regexp-replace* #rx"/$" maybe-dir ""))
    (unless (directory-exists? dir)
      (error 'planet2 "directory does not exist: ~e" dir))
    (match create:format
      ["MANIFEST"
       (with-output-to-file
           (build-path dir "MANIFEST")
         #:exists 'replace
         (λ ()
           (for ([f (in-list (parameterize ([current-directory dir])
                               (find-files file-exists?)))])
             (display f)
             (newline))))]
      [else
       (define pkg (format "~a.~a" dir create:format))
       (define pkg-name
         (regexp-replace
          (regexp (format "~a$" (regexp-quote (format ".~a" create:format))))
          (path->string (file-name-from-path pkg))
          ""))
       (match create:format
         ["tgz"
          (unless (system* (find-executable-path "tar") "-cvzf" pkg "-C" dir ".")
            (delete-file pkg)
            (error 'planet2 "Package creation failed"))]
         ["zip"
          (define orig-pkg (normalize-path pkg))
          (parameterize ([current-directory dir])
            (unless (system* (find-executable-path "zip") "-r" orig-pkg ".")
              (delete-file pkg)
              (error 'planet2 "Package creation failed")))]
         ["plt"
          (pack-plt pkg pkg-name (list dir)
                    #:as-paths (list "."))]
         [x
          (error 'pkg "Invalid package format: ~e" x)])
       (define chk (format "~a.CHECKSUM" pkg))
       (with-output-to-file chk #:exists 'replace
                            (λ () (display (call-with-input-file pkg sha1))))])))

(define dep-behavior/c
  (or/c false/c
        (symbols 'fail 'force 'search-ask 'search-auto)))

(provide
 with-package-lock
 (contract-out
  [current-install-system-wide?
   (parameter/c boolean?)]
  [config-cmd
   (-> boolean? list?
       void)]
  [create-cmd
   (-> string? path-string?
       void)]
  [update-packages
   (->* ((listof string?))
        (#:dep-behavior dep-behavior/c
                        #:all? boolean?
                        #:deps? boolean?)
        void)]
  [remove-packages
   (->* ((listof string?))
        (#:auto? boolean?
                 #:force? boolean?)
        void)]
  [show-cmd
   (-> void)]
  [install-cmd
   (->* ((listof (cons/c boolean? string?)))
        (#:dep-behavior dep-behavior/c
                        #:force? boolean?
                        #:link? boolean?
                        #:ignore-checksums? boolean?)
        void)]))
