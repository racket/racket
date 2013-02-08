#lang racket/base
(require net/url
         json
         openssl/sha1
         racket/contract
         racket/match
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
         racket/string
         file/untgz
         file/tar
         file/zip
         file/unzip
         setup/getinfo
         setup/dirs
         racket/format
         version/utils
         "name.rkt"
         "util.rkt")

(define current-install-system-wide?
  (make-parameter #f))
(define current-install-version-specific?
  (make-parameter #t))
(define current-show-version
  (make-parameter (version)))
(define current-pkg-error
  (make-parameter (lambda args (apply error 'pkg args))))
(define current-no-pkg-db
  (make-parameter #f))

(define (pkg-error . rest)
  (apply (current-pkg-error) rest))

(define (format-list l)
  (if (null? l)
      " [none]"
      (apply string-append
             (for/list ([v (in-list l)])
               (format "\n   ~a" v)))))

(define-logger planet2)

(define (log-exn x what)
  (log-planet2-error (~a "failure ~a\n"
                         "  error: ~s")
                     what
                     (exn-message x)))

(struct pkg-desc (source type name auto?))

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
    (log-planet2-debug "\t\tDownloading ~a to ~a" (url->string url) file)
    (call-with-output-file file
      (λ (op)
        (call/input-url+200
         url
         (λ (ip) (copy-port ip op)))))))

(define (pkg-dir)
  (build-path (cond
               [(current-install-system-wide?) (find-lib-dir)]
               [(current-install-version-specific?)
                (build-path (find-system-path 'addon-dir) (current-show-version))]
               [else
                (find-system-path 'addon-dir)])
              "pkgs"))
(define (pkg-config-file)
  (build-path (pkg-dir) "config.rktd"))
(define (pkg-db-file)
  (build-path (pkg-dir) "pkgs.rktd"))
(define (pkg-installed-dir)
  (build-path (pkg-dir) "installed"))
(define (pkg-lock-file)
  (make-lock-file-name (pkg-db-file)))

(define (link-version-regexp)
  (cond
   [(current-install-system-wide?) #f]
   [(current-install-version-specific?) (regexp (regexp-quote (version)))]
   [else #f]))

(define (make-metadata-namespace)
  (make-base-empty-namespace))

(define (get-metadata metadata-ns pkg-dir key get-default
                      #:checker [checker void])
  (define get-info 
    (with-handlers ([exn:fail? (λ (x)
                                  (log-exn x "getting info")
                                  #f)])
      (get-info/full pkg-dir #:namespace metadata-ns)))
  (define v
    (if get-info
        (get-info key get-default)
        (get-default)))
  (checker v)
  v)

(define (package-collections pkg-dir metadata-ns)
  (for/list ([d (directory-list pkg-dir)]
             #:when (directory-exists? (build-path pkg-dir d))
             #:when (std-filter d))
    d))

(define (package-collection-directories pkg-dir metadata-ns)
  (for/list ([c (in-list (package-collections pkg-dir metadata-ns))])
    (build-path pkg-dir c)))

(define (collection-equal? a b)
  (equal? (if (path? a) a (string->path a))
          (if (path? b) b (string->path b))))

(define (check-dependencies deps)
  (unless (and (list? deps)
               (for/and ([dep (in-list deps)])
                 (define (package-source? dep)
                   (and (string? dep)
                        (package-source->name dep)))
                 (define (version? s)
                   (and (string? s)
                        (valid-version? s)))
                 (or (package-source? dep)
                     (and (list? dep)
                          (= 2 (length dep))
                          (package-source? (car dep))
                          (version? (cadr dep))))))
    (pkg-error (~a "invalid `deps' specification\n"
                   "  specification: ~e")
               deps)))

(define (dependency->name dep)
  (package-source->name
   (dependency->source dep)))

(define (dependency->source dep)
  (if (string? dep)
      dep
      (car dep)))

(define (dependency->version dep)
  (if (string? dep)
      #f
      (cadr dep)))

(define (with-package-lock* read-only? t)
  (define d (pkg-dir))
  (unless read-only? (make-directory* d))
  (if (directory-exists? d)
      ;; If the directory exists, assume that a lock file is
      ;; available or creatable:
      (call-with-file-lock/timeout
       #f (if read-only? 'shared 'exclusive)
       t
       (λ () (pkg-error  (~a "could not acquire package lock\n"
                             "  lock file: ~a")
                         (pkg-lock-file)))
       #:lock-file (pkg-lock-file))
      ;; Directory does not exist; we must be in read-only mode.
      ;; Run `t' under the claim that no database is available
      ;; (in case the database is created concurrently):
      (parameterize ([current-no-pkg-db #t])
        (t))))
(define-syntax-rule (with-package-lock e ...)
  (with-package-lock* #f (λ () e ...)))
(define-syntax-rule (with-package-lock/read-only e ...)
  (with-package-lock* #t (λ () e ...)))

(define (maybe-append lists)
  (and (for/and ([v (in-list lists)]) (not (eq? v 'all)))
       (apply append lists)))

(define (read-pkg-cfg/def k)
  (define c (read-pkg-cfg))
  (hash-ref c k
            (λ ()
              (match k
                ["indexes"
                 (list "https://pkg.racket-lang.org"
                       "https://planet-compat.racket-lang.org")]))))

(define (package-index-lookup pkg)
  (or
   (for/or ([i (in-list (read-pkg-cfg/def "indexes"))])
     (define addr/no-query (combine-url/relative (string->url i)
                                                 (format "pkg/~a" pkg)))
     (define addr (struct-copy url addr/no-query
                               [query (append
                                       (url-query addr/no-query)
                                       (list
                                        (cons 'version (version))))]))
     (log-planet2-debug "resolving via ~a" (url->string addr))
     (call/input-url+200
      addr
      read))
   (pkg-error (~a "cannot find package on indexes\n"
                  "  package: ~a")
              pkg)))

(define (remote-package-checksum pkg)
  (match pkg
    [`(pns ,pkg-name) ; compatibility, for now
     (hash-ref (package-index-lookup pkg-name) 'checksum)]
    [`(pnr ,pkg-name)
     (hash-ref (package-index-lookup pkg-name) 'checksum)]
    [`(url ,pkg-url-str)
     (package-url->checksum pkg-url-str)]))

(define (read-file-hash file)
  (define the-db
    (with-handlers ([exn:fail? (λ (x) 
                                  (log-exn x "reading file hash") 
                                  (hash))])
      (if (file-exists? file) ; don't complain if the file is missing
          (file->value file)
          (hash))))
  the-db)
(define (write-file-hash! file new-db)
  (make-parent-directory* file)
  (with-output-to-file file
    #:exists 'replace
    (λ () (write new-db))))

(define (read-pkg-db)
  (if (current-no-pkg-db)
      #hash()
      (read-file-hash (pkg-db-file))))

(define (package-info pkg-name [fail? #t])
  (define db (read-pkg-db))
  (define pi (hash-ref db pkg-name #f))
  (cond
    [pi
     pi]
    [(not fail?)
     #f]
    [else
     (pkg-error (~a "package not currently installed\n"
                    "  package: ~a\n"
                    "  currently installed:~a")
                pkg-name
                (format-list (hash-keys db)))]))

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

(define (get-default-package-scope)
  (match (get-default-package-scope-as-string)
    ["installation" 'i]
    ["shared" 's]
    [else 'u]))
(define (get-default-package-scope-as-string)
  (parameterize ([current-install-system-wide? #t])
    (define cfg (read-pkg-cfg))
    (hash-ref cfg "default-scope" "user")))

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
            #:version-regexp (link-version-regexp)
            #:root? #t)]
    [_
     (links pkg-dir
            #:remove? #t
            #:user? (not (current-install-system-wide?))
            #:version-regexp (link-version-regexp)
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
  (define metadata-ns (make-metadata-namespace))
  (define pkgs
    (if auto?
      (set->list
       (set-subtract
        (list->set
         (filter
          (λ (p) (pkg-info-auto? (hash-ref db p)))
          all-pkgs))
        (list->set
         (append-map (package-dependencies metadata-ns)
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
        (append-map (package-dependencies metadata-ns)
                    (set->list
                     remaining-pkg-db-set)))))
    (unless (set-empty? deps-to-be-removed)
      (pkg-error (~a "cannot remove packages that are dependencies of other packages\n"
                     "  dependencies:~a")
                 (format-list (set->list deps-to-be-removed)))))
  (for-each remove-package pkgs))

(define (install-packages
         #:old-infos [old-infos empty]
         #:old-descs [old-descs empty]
         #:pre-succeed [pre-succeed void]
         #:dep-behavior [dep-behavior #f]
         #:updating? [updating? #f]
         #:ignore-checksums? [ignore-checksums? #f]
         #:force? [force? #f]
         descs)
  (define check-sums? (not ignore-checksums?))
  (define (install-package pkg 
                           given-type
                           given-pkg-name
                           #:given-checksum [given-checksum #f])    
    (define-values (inferred-pkg-name type) 
      (if (path? pkg)
          (package-source->name+type (path->string pkg)
                                     (or given-type
                                         (if (directory-exists? pkg)
                                             'dir
                                             'file)))
          (package-source->name+type pkg given-type)))
    (define pkg-name (or given-pkg-name inferred-pkg-name))
    (when (and type (not pkg-name))
      (pkg-error (~a "could not infer package name from source\n"
                     "  source: ~a")
                 pkg))
    (cond
     [(and (eq? type 'github)
           (not (regexp-match? #rx"^github://" pkg)))
      ;; Add "github://github.com/"
      (install-package (string-append "github://github.com/" pkg) type 
                       pkg-name #:given-checksum given-checksum)]
     [(or (eq? type 'file-url) (eq? type 'dir-url) (eq? type 'github))
      (define pkg-url (string->url pkg))
      (define scheme (url-scheme pkg-url))

      (define orig-pkg `(url ,pkg))
      (define checksum (remote-package-checksum orig-pkg))
      (define info
        (update-install-info-orig-pkg
         (match type
           ['github
            (when given-checksum
              (set! checksum given-checksum))
            (unless checksum
              (pkg-error 
               (~a "could not find checksum for github package source, which implies it doesn't exist\n"
                   "  source: ~a")
               pkg))
            (match-define (list* user repo branch path)
                          (map path/param-path (url-path/no-slash pkg-url)))
            (define new-url
              (url "https" #f "github.com" #f #t
                   (map (λ (x) (path/param x empty))
                        (list user repo "tarball" checksum))
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
                                     'dir
                                     pkg-name))
                 (λ ()
                    (delete-directory/files tmp-dir))))
             (λ ()
                (delete-directory/files tmp.tgz)))]
           [_
            (define url-last-component
              (path/param-path (last (url-path pkg-url))))
            (define url-looks-like-directory? (eq? type 'dir-url))
            (define-values
              (package-path download-type download-package!)
              (cond
               [url-looks-like-directory?
                (define package-path
                  (make-temporary-file
                   (string-append
                    "~a-"
                    pkg-name)
                   'directory))
                (define (path-like f)
                  (build-path package-path f))
                (define (url-like f)
                  (if (and (pair? (url-path pkg-url))
                           (equal? "" (path/param-path (last (url-path pkg-url)))))
                      ;; normal relative path:
                      (combine-url/relative pkg-url f)
                      ;; we're assuming that the last path element is
                      ;; a directory, so just add f:
                      (struct-copy url pkg-url [path
                                                (append
                                                 (url-path pkg-url)
                                                 (list (path/param f null)))])))
                (values package-path
                        'dir
                        (λ ()
                           (printf "\tCloning remote directory\n")
                           (make-directory* package-path)
                           (define manifest
                             (call/input-url+200
                              (url-like "MANIFEST")
                              port->lines))
                           (unless manifest
                             (pkg-error (~a "could not find MANIFEST for package source\n"
                                            "  source: ~a")
                                        pkg))
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
                        'file
                        (λ ()
                           (log-planet2-debug "\tAssuming URL names a file")
                           (download-file! pkg-url package-path)))]))
            (dynamic-wind
             void
             (λ ()
                (download-package!)
                (log-planet2-debug "\tDownloading done, installing ~a as ~a"
                         package-path pkg-name)
                (install-package package-path
                                 download-type
                                 pkg-name))
             (λ ()
                (when (or (file-exists? package-path)
                          (directory-exists? package-path))
                  (delete-directory/files package-path))))])
         orig-pkg))
      (when (and check-sums?
                 (install-info-checksum info)
                 (not checksum))
        (pkg-error (~a "remote package had no checksum\n"
                       "  package: ~a")
                   pkg))
      (when (and checksum
                 (install-info-checksum info)
                 check-sums?
                 (not (equal? (install-info-checksum info) checksum)))
        (pkg-error (~a "incorrect checksum on package\n"
                       "  package: ~a\n"
                       "  expected ~e\n"
                       "  got ~e")
                   pkg
                   (install-info-checksum info) checksum))
      (update-install-info-checksum
       info
       checksum)]
     [(eq? type 'file)
      (unless (file-exists? pkg)
        (pkg-error "no such file\n  path: ~a" pkg))
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
        (pkg-error (~a "incorrect checksum on package\n"
                       "  expected: ~e\n"
                       "  got: ~e")
                   expected-checksum actual-checksum))
      (define checksum
        actual-checksum)
      (define pkg-format (filename-extension pkg))
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
            [#"tar"
             (untar pkg pkg-dir)]
            [#"gz" ; assuming .tar.gz
             (untar pkg pkg-dir)]
            [#"zip"
             (unzip pkg (make-filesystem-entry-reader #:dest pkg-dir))]
            [#"plt"
             (make-directory* pkg-dir)
             (unpack pkg pkg-dir
                     (lambda (x) (log-planet2-debug "~a" x))
                     (lambda () pkg-dir)
                     #f
                     (lambda (auto-dir main-dir file) pkg-dir))]
            [x
             (pkg-error "invalid package format\n  given: ~a" x)])

          (update-install-info-checksum
           (update-install-info-orig-pkg
            (install-package pkg-dir
                             'dir
                             pkg-name)
            `(file ,(simple-form-path* pkg)))
           checksum))
       (λ ()
          (delete-directory/files pkg-dir)))]
     [(or (eq? type 'dir)
          (eq? type 'link))
      (unless (directory-exists? pkg)
        (pkg-error "no such directory\n  path: ~a" pkg))
      (let ([pkg (directory-path-no-slash pkg)])
        (cond
         [(eq? type 'link)
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
     [(eq? type 'name)
      (define index-info (package-index-lookup pkg))
      (define source (hash-ref index-info 'source))
      (define checksum (hash-ref index-info 'checksum))
      (define info (install-package source
                                    #f
                                    pkg-name
                                    #:given-checksum checksum))
      (when (and (install-info-checksum info)
                 check-sums?
                 (not (equal? (install-info-checksum info) checksum)))
        (pkg-error "incorrect checksum on package\n  package: ~a" pkg))
      (update-install-info-orig-pkg
       (update-install-info-checksum
        info
        checksum)
       `(pnr ,pkg))]
     [else
      (pkg-error "cannot infer package source type\n  source: ~a" pkg)]))
  (define db (read-pkg-db))
  (define db+with-dbs
    (let ([with-sys-wide (lambda (t)
                           (parameterize ([current-install-system-wide? #t])
                             (t)))]
          [with-vers-spec (lambda (t)
                            (parameterize ([current-install-version-specific? #t])
                              (t)))]
          [with-vers-all (lambda (t)
                            (parameterize ([current-install-version-specific? #f])
                              (t)))]
          [with-current (lambda (t) (t))])
      (cond
       [(current-install-system-wide?)
        (list (cons db with-current))]
       [(current-install-version-specific?)
        (list (cons (with-sys-wide read-pkg-db) with-sys-wide)
              (cons db with-current)
              (cons (with-vers-all read-pkg-db) with-vers-all))]
       [else
        (list (cons (with-sys-wide read-pkg-db) with-sys-wide)
              (cons (with-vers-spec read-pkg-db) with-vers-spec)
              (cons db with-current))])))
  (define (install-package/outer infos desc info)
    (match-define (pkg-desc pkg type orig-name auto?) desc)
    (match-define
     (install-info pkg-name orig-pkg pkg-dir clean? checksum)
     info)
    (define name? (or (eq? 'pns (first orig-pkg)) ; compatibility, for now
                      (eq? 'pnr (first orig-pkg))))
    (define (clean!)
      (when clean?
        (delete-directory/files pkg-dir)))
    (define simultaneous-installs
      (for/hash ([i (in-list infos)])
        (values (install-info-name i) (install-info-directory i))))
    (cond
      [(and (not updating?) (package-info pkg-name #f))
       (clean!)
       (pkg-error "package is already installed\n  package: ~a" pkg-name)]
      [(and
        (not force?)
        (for/or ([c (in-list (package-collections pkg-dir metadata-ns))]
                 [d (in-list (package-collection-directories pkg-dir metadata-ns))]
                 #:when #t
                 [f (in-list (directory-list* d))]
                 #:when (member (filename-extension f)
                                (list #"rkt" #"ss")))
          (define (has-collection-file? other-pkg-dir)
            (for/or ([other-c (in-list (package-collections other-pkg-dir metadata-ns))]
                     [other-d (in-list (package-collection-directories other-pkg-dir metadata-ns))])
              (and (collection-equal? c other-c)
                   (file-exists? (build-path other-d f)))))
          (or
           ;; Compare with main installation's collections
           ;;  FIXME: this should check all collection paths that aren't
           ;;  from the package system.
           (and (file-exists? (build-path (find-collects-dir) c f))
                (cons "racket" (build-path c f)))
           ;; Compare with installed packages
           (for*/or ([db+with-db (in-list db+with-dbs)]
                     [other-pkg (in-hash-keys (car db+with-db))]
                     #:unless (and updating? (equal? other-pkg pkg-name)))
             (and ((cdr db+with-db)
                   (lambda () (has-collection-file? (package-directory other-pkg))))
                  (cons other-pkg (build-path c f))))
           ;; Compare with simultaneous installs
           (for/or ([other-pkg-info (in-list infos)]
                    #:unless (eq? other-pkg-info info))
             (and (has-collection-file? (install-info-directory other-pkg-info))
                  (cons (install-info-name other-pkg-info) (build-path c f)))))))
       =>
       (λ (conflicting-pkg*file)
         (clean!)
         (match-define (cons conflicting-pkg file) conflicting-pkg*file)
         (pkg-error (~a "packages conflict\n"
                        "  package: ~a\n"
                        "  package: ~a\n"
                        "  file: ~a")
                    pkg conflicting-pkg file))]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define deps (get-metadata metadata-ns pkg-dir 
                                     'deps (lambda () empty)
                                     #:checker check-dependencies))
          (define unsatisfied-deps
            (map dependency->source
                 (filter-not (λ (dep)
                                (define name (dependency->name dep))
                                (or (equal? name "racket")
                                    (hash-ref simultaneous-installs name #f)
                                    (hash-has-key? db name)))
                             deps)))
          (and (not (empty? unsatisfied-deps))
               unsatisfied-deps)))
       =>
       (λ (unsatisfied-deps)
          (match
             (or dep-behavior
                 (if name?
                   'search-ask
                   'fail))
           ['fail
            (clean!)
            (pkg-error (~a "missing dependencies\n"
                           " for package: ~a\n"
                           " missing packages:~a")
                       pkg
                       (format-list unsatisfied-deps))]
           ['search-auto
            (printf (string-append
                     "The following packages are listed as dependencies, but are not currently installed,\n"
                     "so they will be automatically installed:\n"))
            (printf "\t")
            (for ([p (in-list unsatisfied-deps)])
              (printf "~a " p))
            (printf "\n")
            (raise (vector updating? infos unsatisfied-deps void))]
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
                 (raise (vector updating? infos unsatisfied-deps void))]
                [(or "n" "N")
                 (clean!)
                 (pkg-error "missing dependencies\n  missing packages:~a" (format-list unsatisfied-deps))]
                [x
                 (eprintf "Invalid input: ~e\n" x)
                 (loop)]))]))]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define deps (get-metadata metadata-ns pkg-dir 
                                     'deps (lambda () empty)
                                     #:checker check-dependencies))
          (define update-deps
            (filter-map (λ (dep)
                          (define name (dependency->name dep))
                          (define req-vers (dependency->version dep))
                          (define-values (inst-vers* can-try-update?)
                            (cond
                             [(not req-vers)
                              (values #f #f)]
                             [(equal? name "racket")
                              (values (version) #f)]
                             [(hash-ref simultaneous-installs name #f)
                              => (lambda (dir)
                                   (values
                                    (get-metadata metadata-ns dir
                                                  'version (lambda () "0.0"))
                                    #f))]
                             [else
                              (values (get-metadata metadata-ns (package-directory name)
                                                    'version (lambda () "0.0"))
                                      #t)]))
                          (define inst-vers (if (and req-vers
                                                     (not (and (string? inst-vers*)
                                                               (valid-version? inst-vers*))))
                                                (begin
                                                  (log-planet2-error
                                                   "bad verson specification for ~a: ~e"
                                                   name
                                                   inst-vers*)
                                                  "0.0")
                                                inst-vers*))
                          (and req-vers
                               ((version->integer req-vers) 
                                . > .
                                (version->integer inst-vers))
                               (list name can-try-update? inst-vers req-vers)))
                        deps))
          (and (not (empty? update-deps))
               update-deps)))
       => (lambda (update-deps)
            (define (report-mismatch update-deps)
              (define multi? (1 . < . (length update-deps)))
              (pkg-error (~a "version mismatch for dependenc~a\n"
                             "  for package: ~a\n"
                             "  mismatch packages:~a")
                         (if multi? "ies" "y")
                         pkg
                         (format-deps update-deps)))
            (define (format-deps update-deps)
              (format-list (for/list ([ud (in-list update-deps)])
                             (format "~a (have ~a, need ~a)"
                                     (car ud)
                                     (caddr ud)
                                     (cadddr ud)))))
            ;; If there's a mismatch that we can't attempt to update, complain.
            (unless (andmap cadr update-deps)
              (report-mismatch (filter (compose not cadr) update-deps)))
            ;; Try updates:
            (define update-pkgs (map car update-deps))
            (define (make-pre-succeed)
              (let ([to-update (filter-map update-package update-pkgs)])
                (log-error "to update ~s" to-update)
                (λ () (for-each (compose remove-package pkg-desc-name) to-update))))
            (match (or dep-behavior
                       (if name?
                           'search-ask
                           'fail))
              ['fail
               (clean!)
               (report-mismatch update-deps)]
              ['search-auto
               (printf (string-append
                        "The following packages are listed as dependencies, but are not at the required\n"
                        "version, so they will be automatically updated:~a\n")
                       (format-deps update-deps))
               (raise (vector #t infos update-pkgs (make-pre-succeed)))]
              ['search-ask
               (printf (~a "The following packages are listed as dependencies, but are not at the required\n"
                           "versions:~a\n")
                       (format-deps update-deps))
               (let loop ()
                 (printf "Would you like to update them via your package indices? [Yn] ")
                 (flush-output)
                 (match (read-line)
                   [(or "y" "Y" "")
                    (raise (vector #t infos update-pkgs (make-pre-succeed)))]
                   [(or "n" "N")
                    (clean!)
                    (report-mismatch update-deps)]
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
         (log-planet2-debug "creating link to ~e" final-pkg-dir)
         (links final-pkg-dir
                #:user? (not (current-install-system-wide?))
                #:version-regexp (link-version-regexp)
                #:root? #t)
         (define this-pkg-info
           (pkg-info orig-pkg checksum auto?))
         (log-planet2-debug "updating db with ~e to ~e" pkg-name this-pkg-info)
         (update-pkg-db! pkg-name this-pkg-info))]))
  (define metadata-ns (make-metadata-namespace))
  (define infos
    (for/list ([v (in-list descs)])
      (install-package (pkg-desc-source v) (pkg-desc-type v) (pkg-desc-name v))))
  (define setup-collects
    (maybe-append
     (for/list ([info (in-list (append old-infos infos))])
       (define pkg-dir (install-info-directory info))
       (get-metadata metadata-ns pkg-dir 
                     'setup-collects (lambda () (package-collections
                                                 pkg-dir
                                                 metadata-ns))
                     #:checker (lambda (v)
                                 (unless (or (eq? v 'all)
                                             (and (list? v)
                                                  (for ([c (in-list v)])
                                                    (or (path-string? c)
                                                        (and (list? c)
                                                             (pair? c)
                                                             (andmap path-string? c))))))
                                   (pkg-error "bad 'setup-collects value\n  value: ~e"
                                              v)))))))
  (define do-its
    (map (curry install-package/outer (append old-infos infos))
         (append old-descs descs)
         (append old-infos infos)))
  (pre-succeed)
  (for-each (λ (t) (t)) do-its)
  setup-collects)

(define (install-cmd descs
                     #:old-infos [old-infos empty]
                     #:old-auto+pkgs [old-descs empty]
                     #:force? [force #f]
                     #:ignore-checksums? [ignore-checksums #f]
                     #:pre-succeed [pre-succeed void]
                     #:dep-behavior [dep-behavior #f]
                     #:updating? [updating? #f])
  (with-handlers* ([vector?
                    (match-lambda
                     [(vector updating? new-infos deps more-pre-succeed)
                      (install-cmd
                       #:old-infos new-infos
                       #:old-auto+pkgs (append old-descs descs)
                       #:force? force
                       #:ignore-checksums? ignore-checksums
                       #:dep-behavior dep-behavior
                       #:pre-succeed (lambda () (pre-succeed) (more-pre-succeed))
                       #:updating? updating?
                       (for/list ([dep (in-list deps)])
                         (pkg-desc dep #f #f #t)))])])
    (install-packages
     #:old-infos old-infos
     #:old-descs old-descs
     #:force? force
     #:ignore-checksums? ignore-checksums
     #:dep-behavior dep-behavior
     #:pre-succeed pre-succeed
     #:updating? updating?
     descs)))

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
     (pkg-error (~a "cannot update linked packages\n"
                    "  package name: ~a\n"
                    "  package source: ~a")
                pkg-name
                orig-pkg)]
    [`(dir ,_)
     (pkg-error (~a "cannot update packages installed locally;\n"
                    " package was installed via a local directory\n"
                    "  package name: ~a")
                pkg-name)]
    [`(file ,_)
     (pkg-error (~a "cannot update packages installed locally;\n"
                    " package was installed via a local file\n"
                    "  package name: ~a")
                pkg-name)]
    [`(,_ ,orig-pkg-source)
     (define new-checksum
       (remote-package-checksum orig-pkg))
     (and new-checksum
          (not (equal? checksum new-checksum))
          ;; FIXME: the type shouldn't be #f here; it should be
          ;; preseved from install time:
          (pkg-desc orig-pkg-source #f pkg-name auto?))]))

(define ((package-dependencies metadata-ns) pkg-name)
  (get-metadata metadata-ns (package-directory pkg-name) 
                'deps (lambda () empty)
                #:checker check-dependencies))

(define (update-packages in-pkgs
                         #:all? [all? #f]
                         #:dep-behavior [dep-behavior #f]
                         #:deps? [deps? #f])
  (define metadata-ns (make-metadata-namespace))
  (define pkgs
    (cond
      [(and all? (empty? in-pkgs))
       (filter update-is-possible? (hash-keys (read-pkg-db)))]
      [deps?
       (append-map
        (package-dependencies metadata-ns)
        in-pkgs)]
      [else
       in-pkgs]))
  (define to-update (filter-map update-package pkgs))
  (cond
    [(empty? to-update)
     (printf "No updates available\n")
     #f]
    [else
     (install-cmd
      #:updating? #t
      #:pre-succeed (λ () (for-each (compose remove-package pkg-desc-name) to-update))
      #:dep-behavior dep-behavior
      to-update)]))

(define (show-cmd indent)
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (if (null? pkgs)
        (printf " [none]\n")
        (table-display
         (list*
          (list (format "~aPackage[*=auto]" indent) "Checksum" "Source")
          (for/list ([pkg (in-list pkgs)])
            (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
            (list (format "~a~a~a"
                          indent
                          pkg
                          (if auto?
                              "*"
                              ""))
                  (format "~a" checksum)
                  (format "~a" orig-pkg))))))))

(define (config-cmd config:set key+vals)
  (cond
    [config:set
     (match key+vals
       [(list* (and key "indexes") val)
        (update-pkg-cfg! "indexes" val)]
       [(list (and key "default-scope") val)
        (unless (member val '("installation" "user" "shared"))
          (pkg-error (~a "invliad value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: installation, user, or shared")
                     key
                     val))
        (if (current-install-system-wide?)
            (update-pkg-cfg! "default-scope" val)
            (pkg-error (~a "config key makes sense only with --installation/-i\n"
                           "  config key: ~a\n"
                           "  given value: ~a")
                       key
                       val))]
       [(list key)
        (pkg-error "unsupported config key\n  key: ~e" key)]
       [(list)
        (pkg-error "config key not provided")])]
    [else
     (match key+vals
       [(list key)
        (match key
          ["indexes"
           (for ([s (in-list (read-pkg-cfg/def "indexes"))])
             (printf "~a\n" s))]
          ["default-scope"
           (if (current-install-system-wide?)
               (printf "~a\n" (get-default-package-scope-as-string))
               (pkg-error (~a "config key makes sense only with --installation/-i\n"
                           "  config key: ~a")
                          key))]
          [_
           (pkg-error "unsupported config key\n  key: ~e" key)])]
       [(list)
        (pkg-error "config key not provided")]
       [_
        (pkg-error "multiple config keys provided")])]))

(define (create-cmd create:format maybe-dir)
  (begin
    (define dir (regexp-replace* #rx"/$" maybe-dir ""))
    (unless (directory-exists? dir)
      (pkg-error "directory does not exist\n  path: ~a" dir))
    (match create:format
      ['MANIFEST
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
         ['tgz
          (define pkg/complete (path->complete-path pkg))
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (apply tar-gzip pkg/complete (directory-list))))]
         ['zip
          (define pkg/complete (path->complete-path pkg))
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (apply zip pkg/complete (directory-list))))]
         ['plt
          (define dest (path->complete-path pkg))
          (parameterize ([current-directory dir])
            (define names (filter std-filter (directory-list)))
            (define dirs (filter directory-exists? names))
            (pack-plt dest pkg-name 
                      names
                      #:plt-relative? #t
                      #:as-paths (map (lambda (v) (build-path "collects" v)) names)
                      #:collections (map list (map path->string dirs))))]
         [x
          (pkg-error "invalid package format\n  format: ~a" x)])
       (define chk (format "~a.CHECKSUM" pkg))
       (with-output-to-file chk #:exists 'replace
                            (λ () (display (call-with-input-file pkg sha1))))])))

(define dep-behavior/c
  (or/c false/c
        (symbols 'fail 'force 'search-ask 'search-auto)))

(provide
 with-package-lock
 with-package-lock/read-only
 (contract-out
  [current-install-system-wide?
   (parameter/c boolean?)]
  [current-install-version-specific?
   (parameter/c boolean?)]
  [current-show-version
   (parameter/c string?)]
  [current-pkg-error 
   (parameter/c procedure?)]
  [package-directory
   (-> string? path-string?)]
  [pkg-desc 
   (-> string? 
       (or/c #f 'file 'dir 'link 'file-url 'dir-url 'github 'name) 
       (or/c string? #f) 
       boolean?
       pkg-desc?)]
  [config-cmd
   (-> boolean? list?
       void?)]
  [create-cmd
   (-> (or/c 'zip 'tgz 'plt 'MANIFEST) path-string?
       void?)]
  [update-packages
   (->* ((listof string?))
        (#:dep-behavior dep-behavior/c
                        #:all? boolean?
                        #:deps? boolean?)
        (or/c #f (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [remove-packages
   (->* ((listof string?))
        (#:auto? boolean?
                 #:force? boolean?)
        void)]
  [show-cmd
   (-> string? void)]
  [install-cmd
   (->* ((listof pkg-desc?))
        (#:dep-behavior dep-behavior/c
                        #:force? boolean?
                        #:ignore-checksums? boolean?)
        (or/c #f (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [get-default-package-scope
   (-> (or/c 'i 'u 's))]))
