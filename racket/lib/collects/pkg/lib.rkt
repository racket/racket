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
         syntax/modcollapse
         "name.rkt"
         "util.rkt"
         "strip.rkt"
         (prefix-in db: "db.rkt"))

(define current-pkg-scope
  (make-parameter 'user))
(define current-pkg-scope-version
  (make-parameter (version)))
(define current-pkg-error
  (make-parameter (lambda args (apply error 'pkg args))))
(define current-no-pkg-db
  (make-parameter #f))
(define current-pkg-catalogs
  (make-parameter #f))

(define (pkg-error . rest)
  (apply (current-pkg-error) rest))

(define (format-list l)
  (if (null? l)
      " [none]"
      (apply string-append
             (for/list ([v (in-list l)])
               (format "\n   ~a" v)))))

(define (log-exn x what)
  (log-pkg-error (~a "failure ~a\n"
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

(define (pretty-module-path mod)
  (if (and (list? mod)
           (= 2 (length mod))
           (eq? (car mod) 'lib)
           (regexp-match? #rx"[.]rkt$" (cadr mod)))
      (string->symbol (regexp-replace #rx"[.]rkt$" (cadr mod) ""))
      mod))

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
    (log-pkg-debug "\t\tDownloading ~a to ~a" (url->string url) file)
    (call-with-output-file file
      (λ (op)
        (call/input-url+200
         url
         (λ (ip) (copy-port ip op)))))))

(define (pkg-dir config?)
  (build-path (case (current-pkg-scope)
               [(installation) (if config?
                                   (find-config-dir)
                                   (find-lib-dir))]
               [(user)
                (build-path (find-system-path 'addon-dir) (current-pkg-scope-version))]
               [(shared)
                (find-system-path 'addon-dir)]
               [else (error "unknown package scope")])
              "pkgs"))
(define (pkg-config-file)
  (build-path (pkg-dir #t) "config.rktd"))
(define (pkg-db-file)
  (build-path (pkg-dir #t) "pkgs.rktd"))
(define (pkg-installed-dir)
  (pkg-dir #f))
(define (pkg-lock-file)
  (make-lock-file-name (pkg-db-file)))

(define (link-version-regexp)
  (case (current-pkg-scope)
   [(installation shared) #f]
   [(user) (regexp (regexp-quote (version)))]
   [else (error "unknown package scope")]))

(define (make-metadata-namespace)
  (make-base-empty-namespace))

(define (get-pkg-info pkg-dir metadata-ns)
  (with-handlers ([exn:fail? (λ (x)
                                (log-exn x "getting info")
                                #f)])
    (parameterize ([current-namespace metadata-ns])
      ;; with compiled files on:
      (dynamic-require 'setup/infotab/lang/reader #f)
      (dynamic-require 'setup/infotab 0))
    ;; without compiled files:
    (parameterize ([use-compiled-file-paths '()])
      (get-info/full pkg-dir #:namespace metadata-ns))))

(define (get-metadata metadata-ns pkg-dir key get-default
                      #:checker [checker void])
  (define get-info (get-pkg-info pkg-dir metadata-ns))
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

(define ((check-dependencies which) deps)
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
                          (version? (cadr dep)))
                     (and (list? dep)
                          ((length dep) . >= . 1)
                          (odd? (length dep))
                          (package-source? (car dep))
                          (let loop ([saw (hash)] [dep (cdr dep)])
                            (cond 
                             [(null? dep) #t]
                             [(hash-ref saw (car dep) #f) #f]
                             [else
                              (define kw (car dep))
                              (define val (cadr dep))
                              (and
                               (cond
                                [(eq? kw '#:version) (version? val)]
                                [(eq? kw '#:platform)
                                 (or (string? val)
                                     (regexp? val)
                                     (memq val '(unix windows macosx)))]
                                [else #f])
                               (loop (hash-set saw (car dep) #t)
                                     (cddr dep)))]))))))
    (pkg-error (~a "invalid `" which "' specification\n"
                   "  specification: ~e")
               deps)))

(define (get-all-deps metadata-ns pkg-dir)
  (append
   (get-metadata metadata-ns pkg-dir 
                 'deps (lambda () empty)
                 #:checker (check-dependencies 'deps))
   (get-metadata metadata-ns pkg-dir 
                 'build-deps (lambda () empty)
                 #:checker (check-dependencies 'build-deps))))

(define (dependency->name dep)
  (package-source->name
   (dependency->source dep)))

(define (dependency->source dep)
  (if (string? dep)
      dep
      (car dep)))

(define (dependency->version dep)
  (cond
   [(string? dep) #f]
   [(null? (cdr dep)) #f]
   [(keyword? (cadr dep))
    (dependency-lookup '#:version dep)]
   [else (cadr dep)]))

(define (dependency-lookup kw dep)
  (cond
   [(string? dep) #f]
   [(null? (cdr dep)) #f]
   [(keyword? (cadr dep))
    (define p (member kw (cdr dep)))
    (and p (cadr p))]
   [else #f]))

(define (dependency-this-platform? dep)
  (define p (dependency-lookup '#:platform dep))
  (if p
      (if (symbol? p)
          (eq? p (system-type))
          (let ([s (path->string (system-library-subpath #f))])
            (if (regexp? p)
                (regexp-match? p s)
                (equal? p s))))
      #t))

(define pkg-lock-held (make-parameter #f))

(define (with-pkg-lock* read-only? t)
  (define mode (if read-only? 'shared 'exclusive))
  (define held-mode (pkg-lock-held))
  (if (or (eq? mode held-mode)
          (eq? 'exclusive held-mode))
      (t)
      (let ([d (pkg-dir #t)])
        (unless read-only? (make-directory* d))
        (if (directory-exists? d)
            ;; If the directory exists, assume that a lock file is
            ;; available or creatable:
            (call-with-file-lock/timeout
             #f 
             mode
             (lambda ()
               (parameterize ([pkg-lock-held mode])
                 (t)))
             (λ () (pkg-error  (~a "could not acquire package lock\n"
                                   "  lock file: ~a")
                               (pkg-lock-file)))
             #:lock-file (pkg-lock-file))
            ;; Directory does not exist; we must be in read-only mode.
            ;; Run `t' under the claim that no database is available
            ;; (in case the database is created concurrently):
            (parameterize ([current-no-pkg-db #t])
              (parameterize ([pkg-lock-held mode])
                (t)))))))
(define-syntax-rule (with-pkg-lock e ...)
  (with-pkg-lock* #f (λ () e ...)))
(define-syntax-rule (with-pkg-lock/read-only e ...)
  (with-pkg-lock* #t (λ () e ...)))

(define (maybe-append lists)
  (and (for/and ([v (in-list lists)]) (not (eq? v 'all)))
       (apply append lists)))

(define (read-pkg-cfg/def k)
  (define c (read-pkg-cfg))
  (hash-ref c k
            (λ ()
              (match k
                ["catalogs"
                 (list "https://pkg.racket-lang.org"
                       "https://planet-compat.racket-lang.org")]))))

(define (pkg-config-catalogs)
  (with-pkg-lock/read-only
   (read-pkg-cfg/def "catalogs")))

(define (pkg-catalogs)
  (or (current-pkg-catalogs)
      (map string->url (read-pkg-cfg/def "catalogs"))))

(define (db-path? p)
  (regexp-match? #rx"[.]sqlite$" (path->bytes p)))

(define (catalog-dispatch i server db dir)
  (cond
   [(equal? "file" (url-scheme i))
    (define path (url->path i))
    (cond
     [(db-path? path)
      (parameterize ([db:current-pkg-catalog-file path])
        (db))]
     [(directory-exists? path) (dir path)]
     [else #f])]
   [else (server i)]))

(define (add-version-query addr/no-query)
  (struct-copy url addr/no-query
               [query (append
                       (url-query addr/no-query)
                       (list
                        (cons 'version (version))))]))

(define (package-catalog-lookup pkg details?)
  (or
   (for/or ([i (in-list (pkg-catalogs))])
     (log-pkg-debug "consulting catalog ~a" (url->string i))
     (catalog-dispatch
      i
      ;; Server:
      (lambda (i)
        (define addr (add-version-query
                      (combine-url/relative i (format "pkg/~a" pkg))))
        (log-pkg-debug "resolving via ~a" (url->string addr))
        (read-from-server
         'package-catalog-lookup
         addr
         (lambda (v) (and (hash? v)
                          (for/and ([k (in-hash-keys v)])
                            (symbol? k))))
         (lambda (s) #f)))
      ;; Local database:
      (lambda ()
        (define pkgs (db:get-pkgs #:name pkg))
        (and (pair? pkgs)
             (db-pkg-info (car pkgs) details?)))
      ;; Local directory:
      (lambda (path)
         (define pkg-path (build-path path "pkg" pkg))
         (and (file-exists? pkg-path)
              (call-with-input-file* pkg-path read)))))
   (pkg-error (~a "cannot find package on catalogs\n"
                  "  package: ~a")
              pkg)))

(define (db-pkg-info pkg details?)
  (if details?
      (let ([tags (db:get-pkg-tags (db:pkg-name pkg)
                                   (db:pkg-catalog pkg))]
            [mods (db:get-pkg-modules (db:pkg-name pkg)
                                      (db:pkg-catalog pkg)
                                      (db:pkg-checksum pkg))]
            [deps (db:get-pkg-dependencies (db:pkg-name pkg)
                                           (db:pkg-catalog pkg)
                                           (db:pkg-checksum pkg))])
        (hash 'name (db:pkg-name pkg)
              'author (db:pkg-author pkg)
              'source (db:pkg-source pkg)
              'checksum (db:pkg-checksum pkg)
              'description (db:pkg-desc pkg)
              'tags tags
              'modules mods
              'dependencies deps))
      (hash 'source (db:pkg-source pkg)
            'checksum (db:pkg-source pkg))))

(define (remote-package-checksum pkg download-printf)
  (match pkg
    [`(catalog ,pkg-name)
     (hash-ref (package-catalog-lookup pkg-name #f) 'checksum)]
    [`(url ,pkg-url-str)
     (package-url->checksum pkg-url-str 
                            #:download-printf download-printf)]))

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
      (let ([the-db (read-file-hash (pkg-db-file))])
        ;; compatibility: map 'pnr to 'catalog:
        (for/hash ([(k v) (in-hash the-db)])
          (values k
                  (if (eq? 'pnr (car (pkg-info-orig-pkg v)))
                      ;; note: legacy 'pnr entry cannot be a single-collection package
                      (struct-copy pkg-info v
                                   [orig-pkg `(catalog ,(cadr (pkg-info-orig-pkg v)))])
                      v))))))

(define (package-info pkg-name [fail? #t])
  (define db (read-pkg-db))
  (define pi (hash-ref db pkg-name #f))
  (cond
    [pi
     pi]
    [(not fail?)
     #f]
    [else
     (pkg-not-installed pkg-name db)]))

;; return the current scope as a string
;; -> (or/c "user" "shared" "installation")
(define (current-scope->string)
  (symbol->string (current-pkg-scope)))

;; prints an error for packages that are not installed
;; pkg-name db -> void
(define (pkg-not-installed pkg-name db)
  (define installation-db
    (parameterize ([current-pkg-scope 'installation])
      (read-pkg-db)))
  (define user-db
    (parameterize ([current-pkg-scope 'user])
      (read-pkg-db)))
  (define shared-db
    (parameterize ([current-pkg-scope 'shared])
      (read-pkg-db)))

  ;; see if the package is installed in any scope
  (define-values (in-install? in-user? in-shared?)
   (values
    (and (hash-ref installation-db pkg-name #f)
         "--installation")
    (and (hash-ref user-db pkg-name #f)
         "--user")
    (and (hash-ref shared-db pkg-name #f)
         "--shared")))

  (define not-installed-msg
   (cond [(or in-user? in-install? in-shared?)
          =>
          (λ (scope-str)
             (~a "could not remove package\n"
                 " package installed in a different scope: "
                 (substring scope-str 2) "\n"
                 " consider using the " scope-str " flag\n"))]
         [else (~a "could not remove package\n"
                   " package not currently installed\n")]))

  (pkg-error (~a not-installed-msg
                    "  current scope: ~a\n"
                    "  package: ~a\n"
                    "  currently installed:~a")
                (current-scope->string)
                pkg-name
                (format-list (hash-keys db))))

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

(define (default-pkg-scope)
  (match (default-pkg-scope-as-string)
    ["installation" 'installation]
    ["shared" 'shared]
    [else 'user]))
(define (default-pkg-scope-as-string)
  (parameterize ([current-pkg-scope 'installation])
    (with-pkg-lock/read-only
     (define cfg (read-pkg-cfg))
     (hash-ref cfg "default-scope" "user"))))

(struct pkg-info (orig-pkg checksum auto?) #:prefab)
(struct sc-pkg-info pkg-info (collect) #:prefab) ; a pkg with a single collection
(struct install-info (name orig-pkg directory clean? checksum module-paths))

(define (update-install-info-orig-pkg if op)
  (struct-copy install-info if
               [orig-pkg op]))
(define (update-install-info-checksum if op)
  (struct-copy install-info if
               [checksum op]))

(define (pkg-directory pkg-name)
  (for/or ([scope (in-list '(user shared installation))])
    (parameterize ([current-pkg-scope scope])
      (with-pkg-lock/read-only
       (pkg-directory* pkg-name)))))

(define (pkg-directory* pkg-name)
  (define info (package-info pkg-name #f))
  (and info
       (let ()
         (match-define (pkg-info orig-pkg checksum _) info)
         (match orig-pkg
           [`(link ,orig-pkg-dir)
            orig-pkg-dir]
           [_
            (build-path (pkg-installed-dir) pkg-name)]))))

(define (path->pkg+subpath given-p)
  (define (explode p)
    (explode-path
     (normal-case-path
      (simple-form-path p))))
  (define (sub-path? < p d)
    (and ((length d) . <= . (length p))
         (for/and ([de (in-list d)]
                   [pe (in-list p)])
           (equal? de pe))))
  (define p (explode given-p))
  (define (build-path* l)
    (if (null? l) 'same (apply build-path l)))
  (for/fold ([pkg #f] [subpath #f]) ([scope (in-list '(user shared installation))]
                                     #:when (not pkg))
    (parameterize ([current-pkg-scope scope])
      (with-pkg-lock/read-only
       (define d (explode (pkg-installed-dir)))
       (cond
        [(sub-path? < p d)
         ;; Under the installation mode's package directory.
         ;; We assume that no one else writes there, so the
         ;; next path element is the package name.
         (define len (length d))
         (values (path-element->string (list-ref p len))
                 (build-path* (list-tail p (add1 len))))]
        [else
         ;; Maybe it's a linked package
         (for/fold ([pkg #f] [subpath #f]) ([(k v) (in-hash (read-pkg-db))]
                                            #:when (not pkg))
           (match (pkg-info-orig-pkg v)
             [`(link ,orig-pkg-dir)
              (define e (explode orig-pkg-dir))
              (if (sub-path? <= p e)
                  (values k (build-path* (list-tail p (length e))))
                  (values #f #f))]
             [else (values #f #f)]))])))))

(define (path->pkg given-p)
  (define-values (pkg rest) (path->pkg+subpath given-p))
  pkg)

(define ((remove-package quiet?) pkg-name)
  (unless quiet?
    (printf "Removing ~a\n" pkg-name))
  (define pi (package-info pkg-name))
  (match-define (pkg-info orig-pkg checksum _) pi)
  (define pkg-dir (pkg-directory* pkg-name))
  (remove-from-pkg-db! pkg-name)
  (match orig-pkg
    [`(link ,_)
     (links pkg-dir
            #:remove? #t
            #:user? (not (eq? (current-pkg-scope) 'installation))
            #:version-regexp (link-version-regexp)
            #:root? (not (sc-pkg-info? pi)))]
    [_
     (links pkg-dir
            #:remove? #t
            #:user? (not (eq? (current-pkg-scope) 'installation))
            #:version-regexp (link-version-regexp)
            #:root? (not (sc-pkg-info? pi)))
     (delete-directory/files pkg-dir)]))

(define (pkg-remove in-pkgs
                    #:force? [force? #f]
                    #:auto? [auto? #f]
                    #:quiet? [quiet? #f])
  (define db (read-pkg-db))
  (define all-pkgs
    (hash-keys db))
  (define all-pkgs-set
    (list->set all-pkgs))
  (define metadata-ns (make-metadata-namespace))
  (define pkgs
    (if auto?
        ;; compute fixpoint:
        (let ([init-drop (set-union
                          (list->set
                           (filter
                            (λ (p) (pkg-info-auto? (hash-ref db p)))
                            all-pkgs))
                          (list->set in-pkgs))])
          (let loop ([drop init-drop]
                     [keep (set-subtract
                            (list->set all-pkgs)
                            init-drop)])
            (define deps
              (list->set
               (append-map (package-dependencies metadata-ns)
                           (set->list keep))))
            (define still-drop (set-subtract drop deps))
            (define delta (set-subtract drop still-drop))
            (if (set-empty? delta)
                (set->list drop)
                (loop still-drop
                      (set-union keep delta)))))
        ;; just given pkgs:
        in-pkgs))
  (define setup-collects
    (get-setup-collects (filter-map pkg-directory* pkgs)
                        metadata-ns))
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
                 (format-list 
                  (map
                   (λ (p)
                     (define ds
                       (filter (λ (dp)
                                 (member p ((package-dependencies metadata-ns) dp)))
                               (set->list
                                remaining-pkg-db-set)))
                     (~a p " (required by: " ds ")"))
                   (set->list deps-to-be-removed))))))
  (for-each (remove-package quiet?) pkgs)
  ;; setup only collections that still exist:
  (and setup-collects
       (for/list ([c (in-list setup-collects)]
                  #:when (apply collection-path
                                (if (path-string? c) (list c) c)
                                #:fail (lambda (s) #f)))
         c)))

;; Downloads a package (if needed) and unpacks it (if needed) into a  
;; temporary directory. 
(define (stage-package/info pkg
                            given-type
                            given-pkg-name
                            #:given-checksum [given-checksum #f]
                            check-sums?
                            download-printf
                            metadata-ns)
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
    (stage-package/info (string-append "github://github.com/" pkg) type 
                        pkg-name 
                        #:given-checksum given-checksum
                        check-sums? download-printf
                        metadata-ns)]
   [(or (eq? type 'file-url) (eq? type 'dir-url) (eq? type 'github))
    (define pkg-url (string->url pkg))
    (define scheme (url-scheme pkg-url))

    (define orig-pkg `(url ,pkg))
    (define checksum (or given-checksum
                         (remote-package-checksum orig-pkg download-printf)))
    (define info
      (update-install-info-orig-pkg
       (match type
         ['github
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
                 (download-printf "Downloading ~a\n" (url->string new-url))
                 (download-file! new-url tmp.tgz)
                 (dynamic-wind
                     void
                     (λ ()
                        (untar tmp.tgz tmp-dir #:strip-components 1)
                        (stage-package/info (path->string package-path)
                                            'dir
                                            pkg-name
                                            #:given-checksum checksum
                                            check-sums?
                                            download-printf
                                            metadata-ns))
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
                         (download-printf "\tCloning remote directory ~a\n"
                                          (url->string pkg-url))
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
                         (log-pkg-debug "\tAssuming URL names a file")
                         (download-file! pkg-url package-path)))]))
          (dynamic-wind
              void
              (λ ()
                 (download-package!)
                 (log-pkg-debug "\tDownloading done, installing ~a as ~a"
                                package-path pkg-name)
                 (stage-package/info package-path
                                     download-type
                                     pkg-name
                                     #:given-checksum checksum
                                     check-sums?
                                     download-printf
                                     metadata-ns))
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
                      (lambda (x) (log-pkg-debug "~a" x))
                      (lambda () pkg-dir)
                      #f
                      (lambda (auto-dir main-dir file) pkg-dir))]
             [x
              (pkg-error "invalid package format\n  given: ~a" x)])

           (update-install-info-checksum
            (update-install-info-orig-pkg
             (stage-package/info pkg-dir
                                 'dir
                                 pkg-name
                                 #:given-checksum checksum
                                 check-sums?
                                 download-printf
                                 metadata-ns)
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
                      #f #f
                      (directory->module-paths pkg pkg-name metadata-ns))]
       [else
        (define pkg-dir
          (make-temporary-file "pkg~a" 'directory))
        (delete-directory pkg-dir)
        (make-parent-directory* pkg-dir)
        (copy-directory/files pkg pkg-dir #:keep-modify-seconds? #t)
        (install-info pkg-name
                      `(dir ,(simple-form-path* pkg))
                      pkg-dir
                      #t #f
                      (directory->module-paths pkg-dir pkg-name metadata-ns))]))]
   [(eq? type 'name)
    (define catalog-info (package-catalog-lookup pkg #f))
    (define source (hash-ref catalog-info 'source))
    (define checksum (hash-ref catalog-info 'checksum))
    (define info (stage-package/info source
                                     #f
                                     pkg-name
                                     #:given-checksum checksum
                                     check-sums?
                                     download-printf
                                     metadata-ns))
    (when (and (install-info-checksum info)
               check-sums?
               (not (equal? (install-info-checksum info) checksum)))
      (pkg-error "incorrect checksum on package\n  package: ~a" pkg))
    (update-install-info-orig-pkg
     (update-install-info-checksum
      info
      checksum)
     `(catalog ,pkg))]
   [else
    (pkg-error "cannot infer package source type\n  source: ~a" pkg)]))

(define (pkg-stage desc
                   #:checksum [checksum #f])
  (define i (stage-package/info (pkg-desc-source desc)
                                (pkg-desc-type desc)
                                (pkg-desc-name desc)
                                #:given-checksum checksum 
                                #t
                                void
                                (make-metadata-namespace)))
  (values (install-info-name i)
          (install-info-directory i)
          (install-info-checksum i)
          (install-info-clean? i)))

(define (install-packages
         #:old-infos [old-infos empty]
         #:old-descs [old-descs empty]
         #:pre-succeed [pre-succeed void]
         #:dep-behavior [dep-behavior #f]
         #:updating? [updating? #f]
         #:ignore-checksums? [ignore-checksums? #f]
         #:skip-installed? [skip-installed? #f]
         #:force? [force? #f]
         #:quiet? [quiet? #f]
         descs)
  (define download-printf (if quiet? void printf))
  (define check-sums? (not ignore-checksums?))
  (define db (read-pkg-db))
  (define db+with-dbs
    (let ([with-sys-wide (lambda (t)
                           (parameterize ([current-pkg-scope 'installation])
                             (t)))]
          [with-vers-spec (lambda (t)
                            (parameterize ([current-pkg-scope 'user])
                              (t)))]
          [with-vers-all (lambda (t)
                            (parameterize ([current-pkg-scope 'shared])
                              (t)))]
          [with-current (lambda (t) (t))])
      (case (current-pkg-scope)
       [(installation)
        (list (cons db with-current))]
       [(user)
        (list (cons (with-sys-wide read-pkg-db) with-sys-wide)
              (cons db with-current)
              (cons (with-vers-all read-pkg-db) with-vers-all))]
       [(shared)
        (list (cons (with-sys-wide read-pkg-db) with-sys-wide)
              (cons (with-vers-spec read-pkg-db) with-vers-spec)
              (cons db with-current))]
       [else (error "unknown package scope")])))
  (define (install-package/outer infos desc info)
    (match-define (pkg-desc pkg type orig-name auto?) desc)
    (match-define
     (install-info pkg-name orig-pkg pkg-dir clean? checksum module-paths)
     info)
    (define name? (eq? 'catalog (first orig-pkg)))
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
        (for/or ([mp (in-set module-paths)])
          ;; In an installed collection? Try resolving the path:
          (define r (with-handlers ([exn:fail:filesystem:missing-module? (lambda (x) #f)])
                      ((current-module-name-resolver) mp #f #f #f)))
          (define f (and r (resolved-module-path-name r)))
          (when f
            (unless (path? f)
              (pkg-error "expected a filesystem path for a resolved module path: ~a" mp)))
          ;; Check for source or compiled:
          (cond
           [(and f
                 (or (file-exists? f)
                     (file-exists? (path-replace-suffix f #".ss"))
                     (let-values ([(base name dir?) (split-path f)])
                       (or (file-exists? (build-path base "compiled" (path-add-suffix name #".zo")))
                           (file-exists? (build-path base "compiled" (path-add-suffix
                                                                      (path-replace-suffix name #".ss")
                                                                      #".zo"))))))
                 (or (not updating?)
                     (not (equal? pkg-name (path->pkg f)))))
            ;; This module is already installed
            (cons (path->pkg f) mp)]
           [else
            ;; Compare with simultaneous installs
            (for/or ([other-pkg-info (in-list infos)]
                     #:unless (eq? other-pkg-info info))
              (and (set-member? (install-info-module-paths other-pkg-info) mp)
                   (cons (install-info-name other-pkg-info) 
                         mp)))])))
       =>
       (λ (conflicting-pkg*mp)
         (clean!)
         (match-define (cons conflicting-pkg mp) conflicting-pkg*mp)
         (if conflicting-pkg
             (pkg-error (~a "packages conflict\n"
                            "  package: ~a\n"
                            "  package: ~a\n"
                            "  module path: ~s")
                        pkg conflicting-pkg (pretty-module-path mp))
             (pkg-error (~a "package conflicts with existing installed\n"
                            "  package: ~a\n"
                            "  module path: ~s")
                        pkg (pretty-module-path mp))))]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define deps (get-all-deps metadata-ns pkg-dir))
          (define unsatisfied-deps
            (map dependency->source
                 (filter-not (λ (dep)
                                (define name (dependency->name dep))
                                (or (equal? name "racket")
                                    (not (dependency-this-platform? dep))
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
              (printf "Would you like to install them via your package catalogs? [Yn] ")
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
          (define deps (get-all-deps metadata-ns pkg-dir))
          (define update-deps
            (filter-map (λ (dep)
                          (define name (dependency->name dep))
                          (define req-vers (dependency->version dep))
                          (define this-platform? (dependency-this-platform? dep))
                          (define-values (inst-vers* can-try-update?)
                            (cond
                             [(not this-platform?)
                              (values #f #f)]
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
                              (values (get-metadata metadata-ns (pkg-directory* name)
                                                    'version (lambda () "0.0"))
                                      #t)]))
                          (define inst-vers (if (and this-platform?
                                                     req-vers
                                                     (not (and (string? inst-vers*)
                                                               (valid-version? inst-vers*))))
                                                (begin
                                                  (log-pkg-error
                                                   "bad verson specification for ~a: ~e"
                                                   name
                                                   inst-vers*)
                                                  "0.0")
                                                inst-vers*))
                          (and this-platform?
                               req-vers
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
              (let ([to-update (filter-map (update-package download-printf) update-pkgs)])
                (λ () (for-each (compose (remove-package quiet?) pkg-desc-name) to-update))))
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
                 (printf "Would you like to update them via your package catalogs? [Yn] ")
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
              (copy-directory/files pkg-dir final-pkg-dir #:keep-modify-seconds? #t)
              (clean!)
              final-pkg-dir]
             [else
              pkg-dir]))
         (define single-collect (pkg-single-collection final-pkg-dir 
                                                       #:namespace metadata-ns))
         (log-pkg-debug "creating ~alink to ~e" 
                        (if single-collect "single-collection " "") 
                        final-pkg-dir)
         (links final-pkg-dir
                #:name single-collect
                #:user? (not (eq? 'installation (current-pkg-scope)))
                #:version-regexp (link-version-regexp)
                #:root? (not single-collect))
         (define this-pkg-info
           (if single-collect
               (sc-pkg-info orig-pkg checksum auto? single-collect)
               (pkg-info orig-pkg checksum auto?)))
         (log-pkg-debug "updating db with ~e to ~e" pkg-name this-pkg-info)
         (update-pkg-db! pkg-name this-pkg-info))]))
  (define metadata-ns (make-metadata-namespace))
  (define infos
    (for/list ([v (in-list descs)])
      (stage-package/info (pkg-desc-source v) (pkg-desc-type v) (pkg-desc-name v) 
                          check-sums? download-printf
                          metadata-ns)))
  (define setup-collects (get-setup-collects (map install-info-directory
                                                  (append old-infos infos))
                                             metadata-ns))
  (define do-its
    (map (curry install-package/outer (append old-infos infos))
         (append old-descs descs)
         (append old-infos infos)))
  (pre-succeed)
  (for-each (λ (t) (t)) do-its)
  setup-collects)

(define (pkg-single-collection dir #:namespace [metadata-ns (make-metadata-namespace)])
  (define i (get-pkg-info dir metadata-ns))
  (and i (let ([s (i 'single-collection (lambda () #f))])
           (and (string? s) 
                s))))

(define (get-setup-collects pkg-directories metadata-ns)
  (maybe-append
   (for/list ([pkg-dir (in-list pkg-directories)])
     (define single-collect
       (pkg-single-collection pkg-dir #:namespace metadata-ns))
     (or (and single-collect (list single-collect))
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
                                                v))))))))

(define (pkg-install descs
                     #:old-infos [old-infos empty]
                     #:old-auto+pkgs [old-descs empty]
                     #:force? [force #f]
                     #:ignore-checksums? [ignore-checksums? #f]
                     #:skip-installed? [skip-installed? #f]
                     #:pre-succeed [pre-succeed void]
                     #:dep-behavior [dep-behavior #f]
                     #:updating? [updating? #f]
                     #:quiet? [quiet? #f])
  (with-handlers* ([vector?
                    (match-lambda
                     [(vector updating? new-infos deps more-pre-succeed)
                      (pkg-install
                       #:old-infos new-infos
                       #:old-auto+pkgs (append old-descs descs)
                       #:force? force
                       #:ignore-checksums? ignore-checksums?
                       #:dep-behavior dep-behavior
                       #:pre-succeed (lambda () (pre-succeed) (more-pre-succeed))
                       #:updating? updating?
                       (for/list ([dep (in-list deps)])
                         (pkg-desc dep #f #f #t)))])])
    (install-packages
     #:old-infos old-infos
     #:old-descs old-descs
     #:force? force
     #:ignore-checksums? ignore-checksums?
     #:skip-installed? skip-installed?
     #:dep-behavior dep-behavior
     #:pre-succeed pre-succeed
     #:updating? updating?
     #:quiet? quiet?
     (if (not skip-installed?)
         descs
         (let ([db (read-pkg-db)])
           (filter (lambda (d)
                     (define pkg-name
                       (or (pkg-desc-name d)
                           (let-values ([(name type)
                                         (package-source->name+type (pkg-desc-source d) 
                                                                    (pkg-desc-type d))])
                             name)))
                     (not (hash-ref db pkg-name #f)))
                   descs))))))

(define (update-is-possible? pkg-name)
  (match-define (pkg-info orig-pkg checksum _)
                (package-info pkg-name))
  (define ty (first orig-pkg))
  (not (member ty '(link dir file))))

(define ((update-package download-printf) pkg-name)
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
       (remote-package-checksum orig-pkg download-printf))
     (and new-checksum
          (not (equal? checksum new-checksum))
          ;; FIXME: the type shouldn't be #f here; it should be
          ;; preseved from install time:
          (pkg-desc orig-pkg-source #f pkg-name auto?))]))

(define ((package-dependencies metadata-ns) pkg-name)  
  (get-all-deps metadata-ns (pkg-directory* pkg-name)))

(define (pkg-update in-pkgs
                    #:all? [all? #f]
                    #:dep-behavior [dep-behavior #f]
                    #:deps? [deps? #f]
                    #:quiet? [quiet? #f])
  (define download-printf (if quiet? void printf))
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
  (define to-update (filter-map (update-package download-printf) pkgs))
  (cond
    [(empty? to-update)
     (printf "No updates available\n")
     null]
    [else
     (printf "Updating: ~a\n" to-update)
     (pkg-install
      #:updating? #t
      #:pre-succeed (λ () (for-each (compose (remove-package quiet?) pkg-desc-name) to-update))
      #:dep-behavior dep-behavior
      #:quiet? quiet?
      to-update)]))

(define (pkg-show indent #:directory? [dir? #f])
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (if (null? pkgs)
        (printf " [none]\n")
        (table-display
         (list*
          (list* (format "~aPackage[*=auto]" indent) "Checksum" "Source"
                 (if dir?
                   (list "Directory")
                   empty))
          (for/list ([pkg (in-list pkgs)])
            (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
            (list* (format "~a~a~a"
                           indent
                           pkg
                           (if auto?
                             "*"
                             ""))
                   (format "~a" checksum)
                   (format "~a" orig-pkg)
                   (if dir?
                     (list (~a (pkg-directory* pkg)))
                     empty))))))))

(define (installed-pkg-table #:scope [given-scope #f])
  (parameterize ([current-pkg-scope 
                  (or given-scope (default-pkg-scope))])
    (with-pkg-lock/read-only
     (read-pkg-db))))

(define (installed-pkg-names #:scope [given-scope #f])
  (sort (hash-keys (installed-pkg-table #:scope given-scope))
        string-ci<=?))
  
(define (pkg-config config:set key+vals)
  (cond
    [config:set
     (match key+vals
       [(list* (and key "catalogs") val)
        (update-pkg-cfg! "catalogs" val)]
       [(list (and key "default-scope") val)
        (unless (member val '("installation" "user" "shared"))
          (pkg-error (~a "invliad value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: installation, user, or shared")
                     key
                     val))
        (if (eq? 'installation (current-pkg-scope))
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
          ["catalogs"
           (for ([s (in-list (read-pkg-cfg/def "catalogs"))])
             (printf "~a\n" s))]
          ["default-scope"
           (if (eq? 'installation (current-pkg-scope))
               (printf "~a\n" (default-pkg-scope-as-string))
               (pkg-error (~a "config key makes sense only with --installation/-i\n"
                           "  config key: ~a")
                          key))]
          [_
           (pkg-error "unsupported config key\n  key: ~e" key)])]
       [(list)
        (pkg-error "config key not provided")]
       [_
        (pkg-error "multiple config keys provided")])]))

(define (create-as-is create:format pkg-name dir orig-dir
                      #:quiet? [quiet? #f]
                      #:hide-src? [hide-src? #f]
                      #:dest [dest-dir #f])
  (begin
    (unless (directory-exists? dir)
      (pkg-error "directory does not exist\n  path: ~a" dir))
    (match create:format
      ['MANIFEST
       (unless quiet?
         (printf "creating manifest for ~a\n"
                 orig-dir))
       (with-output-to-file (build-path (or dest-dir dir) "MANIFEST")
         #:exists 'replace
         (λ ()
           (for ([f (in-list (parameterize ([current-directory dir])
                               (find-files file-exists?)))])
             (display f)
             (newline))))]
      [else
       (define pkg (format "~a.~a" pkg-name create:format))
       (define actual-dest-dir (or dest-dir 
                                   (let-values ([(base name dir?) (split-path dir)])
                                     (cond
                                      [(path? base) (path->complete-path base)]
                                      [else (current-directory)]))))
       (define pkg/complete (path->complete-path pkg actual-dest-dir))
       (unless quiet?
         (printf "packing~a into ~a\n"
                 (if hide-src? "" (format " ~a" dir))
                 (if dest-dir
                     pkg/complete
                     pkg)))
       (match create:format
         ['tgz
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (apply tar-gzip pkg/complete (directory-list))))]
         ['zip
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (apply zip pkg/complete (directory-list))))]
         ['plt
          (define dest pkg/complete)
          (when (pkg-single-collection dir)
            (pkg-error (~a "single-collection package not supported in .plt format\n"
                           "  directory: ~a")
                       dir))
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
       (define chk/complete (path->complete-path chk actual-dest-dir))
       (unless quiet?
         (printf "writing package checksum to ~a\n"
                 (if dest-dir
                     chk/complete
                     chk)))
       (with-output-to-file chk/complete
         #:exists 'replace
         (λ () (display (call-with-input-file pkg/complete sha1))))])))

(define (stripped-create mode name dir
                        #:format [create:format 'zip]
                        #:quiet? [quiet? #f]
                        #:dest [archive-dest-dir #f])
  (define tmp-dir (make-temporary-file "create-binary-~a" 'directory))
  (dynamic-wind
      void
      (lambda ()
        (define dest-dir (build-path tmp-dir name))
        (make-directory dest-dir)
        (generate-stripped-directory mode dir dest-dir)
        (create-as-is create:format name dest-dir dir 
                      #:hide-src? #t 
                      #:quiet? quiet?
                      #:dest archive-dest-dir))
      (lambda ()
        (delete-directory/files tmp-dir))))

(define (pkg-create create:format dir-or-name
                    #:dest [dest-dir #f]
                    #:source [source 'dir]
                    #:mode [mode 'as-is]
                    #:quiet? [quiet? #f])
  (define pkg-name
    (if (eq? source 'dir)
        (path->string (let-values ([(base name dir?) (split-path dir-or-name)])
                        name))
        dir-or-name))
  (define dir
    (if (eq? source 'dir)
        dir-or-name
        (let ()
          (define (get-dir scope)
            (parameterize ([current-pkg-scope scope])
              (with-pkg-lock/read-only
               (pkg-directory* dir-or-name))))
          (define dir (or (get-dir 'user)
                          (get-dir 'shared)))
          (unless dir
            (pkg-error (~a "package not installed in user or shared scope\n"
                           "  package name: ~a"
                           (if (get-dir 'installation)
                               "\n  installed in scope: installation"
                               ""))
                       dir-or-name))
          dir)))
  (case mode
    [(as-is) (create-as-is create:format pkg-name dir dir
                           #:dest dest-dir
                           #:quiet? quiet?)]
    [else (stripped-create mode pkg-name dir
                           #:dest dest-dir
                           #:format create:format
                           #:quiet? quiet?)]))

(define (pkg-catalog-copy srcs dest
                        #:from-config? [from-config? #f]
                        #:merge? [merge? #f]
                        #:force? [force? #f]
                        #:override? [override? #f])
  (define src-paths
    (for/list ([src (in-list (append srcs
                                     (if from-config?
                                         (pkg-config-catalogs)
                                         null)))])
      (define src-path
        (cond
         [(path? src) (path->complete-path src)]
         [(regexp-match? #rx"^https?://" src)
          (string->url src)]
         [(regexp-match? #rx"^file://" src)
          (url->path (string->url src))]
         [(regexp-match? #rx"^[a-zA-Z]*://" src)
          (pkg-error (~a "unrecognized URL scheme for an catalog\n"
                         "  URL: ~a")
                     src)]
         [else (path->complete-path src)]))
      (when (path? src-path)
        (cond
         [(db-path? src-path)
          (void)]
         [(directory-exists? src-path)
          (void)]
         [(let-values ([(base name dir?) (split-path src-path)]) dir?)
          (void)]
         [else
          (pkg-error (~a "bad source catalog path\n"
                         "  path: ~a\n"
                         "  expected: directory or path with \".sqlite\" extension")
                     src)]))
      src-path))
  (define dest-path
    (cond
     [(path? dest) (path->complete-path dest)]
     [(regexp-match? #rx"^file://" dest)
      (url->path (string->url dest))]
     [(regexp-match? #rx"^[a-zA-Z]*://" dest)
      (pkg-error (~a "cannot copy to a non-file destination catalog\n"
                     "  given URL: ~a")
                 dest)]
     [else (path->complete-path dest)]))

  (unless (or force? merge?)
    (when (or (file-exists? dest-path)
              (directory-exists? dest-path)
              (link-exists? dest-path))
      (pkg-error (~a "destination exists\n"
                     "  path: ~a")
                 dest-path)))

  (define details
    (let ([src-paths (if (and merge?
                              (or (file-exists? dest-path)
                                  (directory-exists? dest-path)))
                         (if override?
                             (append src-paths
                                     (list dest-path))
                             (cons dest-path
                                   src-paths))
                         src-paths)])
      (parameterize ([current-pkg-catalogs (for/list ([src-path src-paths])
                                            (if (path? src-path)
                                                (path->url src-path)
                                                src-path))])
        (get-all-pkg-details-from-catalogs))))

  (when (and force? (not merge?))
    (cond
     [(file-exists? dest-path)
      (delete-file dest-path)]
     [(directory-exists? dest-path)
      (if (db-path? dest-path)
          (delete-directory/files dest-path)
          (for ([i (directory-list dest-path)])
            (delete-directory/files (build-path dest-path i))))]
     [(link-exists? dest-path)
      (delete-file dest-path)]))

  (cond
   [(db-path? dest-path)
    (parameterize ([db:current-pkg-catalog-file dest-path])
      (db:set-catalogs! '("local"))
      (db:set-pkgs! "local"
                    (for/list ([(k v) (in-hash details)])
                      (db:pkg k "local"
                              (hash-ref v 'author "")
                              (hash-ref v 'source "")
                              (hash-ref v 'checksum "")
                              (hash-ref v 'description ""))))
      (for ([(k v) (in-hash details)])
        (define t (hash-ref v 'tags '()))
        (unless (null? t)
          (db:set-pkg-tags! k "local" t)))
      (for ([(k v) (in-hash details)])
        (define mods (hash-ref v 'modules '()))
        (unless (null? mods)
          (define cs (hash-ref v 'checksum ""))
          (db:set-pkg-modules! k "local" cs mods)))
      (for ([(k v) (in-hash details)])
        (define deps (hash-ref v 'dependencies '()))
        (unless (null? deps)
          (define cs (hash-ref v 'checksum ""))
          (db:set-pkg-dependencies! k "local" cs deps))))]
   [else
    (define pkg-path (build-path dest-path "pkg"))
    (make-directory* pkg-path)
    (for ([(k v) (in-hash details)])
      (call-with-output-file*
       #:exists 'truncate/replace
       (build-path pkg-path k)
       (lambda (o) (write v o))))
    (call-with-output-file*
     #:exists 'truncate/replace
     (build-path dest-path "pkgs")
     (lambda (o) (write (hash-keys details) o)))
    (call-with-output-file*
     #:exists 'truncate/replace
     (build-path dest-path "pkgs-all")
     (lambda (o) (write details o)))]))

(define (pkg-catalog-show names 
                        #:all? [all? #f]
                        #:only-names? [only-names? #f]
                        #:modules? [modules? #f])
  (for ([name (in-list names)])
    (define-values (parsed-name type)
      (package-source->name+type name #f))
    (unless (eq? type 'name)
      (pkg-error (~a "incorrect syntax for a package name\n"
                     "  given: ~a")
                 name)))
  
  (cond
   [only-names?
    (define all-names (if all?
                          (get-all-pkg-names-from-catalogs)
                          names))
    (for ([name (in-list all-names)])
      (unless all?
        ;; Make sure it's available:
        (get-pkg-details-from-catalogs name))
      (printf "~a\n" name))]
   [else
    (define all-details (and all?
                             (get-all-pkg-details-from-catalogs)))
    (for ([name (in-list (if all?
                             (hash-keys all-details)
                             names))])
      (define details (if all?
                          (hash-ref all-details name)
                          (get-pkg-details-from-catalogs name)))
      (printf "Package name: ~a\n" name)
      (for ([key '(author source checksum tags description)])
        (define v (hash-ref details key #f))
        (when v
          (printf " ~a: ~a\n"
                  (string-titlecase (symbol->string key))
                  (if (list? v)
                      (apply ~a #:separator ", " v)
                      v))))
      (for ([key '(dependencies)])
        (define v (hash-ref details key null))
        (unless (null? v)
          (printf " Dependencies:\n")
          (for ([dep (in-list v)])
            (define vers (dependency->version dep))
            (define plat (dependency-lookup '#:platform dep))
            (printf "  ~a~a~a\n"
                    (dependency->name dep)
                    (if vers
                        (format " version ~a" vers)
                        "")
                    (if plat
                        (format " on platform ~v" plat)
                        "")))))
      (when modules?
        (printf "Modules:")
        (for/fold ([col 72]) ([mod (in-list (hash-ref details 'modules null))])
          (define pretty-mod (pretty-module-path mod))
          (define mod-str (~a " " pretty-mod))
          (define new-col (if ((+ col (string-length mod-str)) . > . 72)
                              (begin
                                (newline)
                                0)
                              col))
          (display mod-str)
          (+ new-col (string-length mod-str)))
        (newline)))]))
  
(define (get-all-pkg-names-from-catalogs)
  (define ht
    (for*/hash ([i (in-list (pkg-catalogs))]
                [name
                 (catalog-dispatch
                  i
                  ;; Server:
                  (lambda (i)
                    (read-from-server 
                     'get-all-pkg-names-from-catalogs
                     (add-version-query
                      (combine-url/relative i "pkgs"))
                     (lambda (l) (and (list? l) 
                                      (andmap string? l)))))
                  ;; Local database:
                  (lambda ()
                    (map db:pkg-name (db:get-pkgs)))
                  ;; Local directory:
                  (lambda (path)
                    (define pkgs-path (build-path path "pkgs"))
                    (cond
                     [(file-exists? pkgs-path)
                      (call-with-input-file* pkgs-path read)]
                     [else
                      (define pkg-path (build-path path "pkg"))
                      (for/list ([i (directory-list pkg-path)]
                                 #:when (file-exists? (build-path pkg-path i)))
                        (path-element->string i))])))])
      (values name #t)))
  (sort (hash-keys ht) string<?))

(define (get-pkg-details-from-catalogs name)
  (for/or ([i (in-list (pkg-catalogs))])
    (package-catalog-lookup name #t)))

(define (get-all-pkg-details-from-catalogs)
  (for/fold ([ht (hash)]) ([i (in-list (pkg-catalogs))])
    (define one-ht
      (catalog-dispatch
       i
       ;; Server:
       (lambda (i)
         (read-from-server
          'get-all-pkg-details-from-catalogs
          (add-version-query
           (combine-url/relative i "pkgs-all"))
          (lambda (v)
            (and (hash? v)
                 (for/and ([(k v) (in-hash v)])
                   (and (string? k)
                        (hash? v)
                        (for/and ([k (in-hash-keys v)])
                          (symbol? k))))))))
       ;; Local database:
       (lambda ()
         (define pkgs (db:get-pkgs))
         (for/fold ([ht (hash)]) ([p (in-list pkgs)])
           (if (hash-ref ht (db:pkg-name p) #f)
               ht
               (hash-set ht
                         (db:pkg-name p)
                         (db-pkg-info p #t)))))
       ;; Local directory:
       (lambda (path)
         (define pkgs-all-path (build-path path "pkgs-all"))
         (cond
          [(file-exists? pkgs-all-path)
           (call-with-input-file* pkgs-all-path read)]
          [else
           (define pkg-path (build-path path "pkg"))
           (for/hash ([i (directory-list pkg-path)]
                      #:when (file-exists? (build-path pkg-path i)))
             (values (path-element->string i)
                     (call-with-input-file* (build-path pkg-path i)
                                            read)))]))))
    (for/fold ([ht ht]) ([(k v) (in-hash one-ht)])
      (if (hash-ref ht k #f)
          ht
          (hash-set ht k v)))))

(define (extract-dependencies get-info)
  (define v (if get-info
                (get-info 'deps (lambda () empty))
                empty))
  ((check-dependencies 'deps) v)
  (define v2 (if get-info
                (get-info 'build-deps (lambda () empty))
                empty))
  ((check-dependencies 'build-deps) v2)
  (append v v2))

(define (get-pkg-content desc 
                         #:extract-info [extract-info extract-dependencies])
  (define-values (pkg-name dir cksum clean?) (pkg-stage desc))
  (define metadata-ns (make-metadata-namespace))
  (define get-info (get-info/full dir #:namespace metadata-ns))
  (define module-paths
    (set->list (directory->module-paths dir pkg-name metadata-ns)))
  (begin0
   (values cksum
           module-paths
           (extract-info get-info))
   (when clean?
     (delete-directory/files dir))))

(define (directory->module-paths dir pkg-name metadata-ns)
  (define dummy (build-path dir "dummy.rkt"))
  (define compiled (string->path-element "compiled"))
  (define single-collect (pkg-single-collection dir #:namespace metadata-ns))
  (define (try-path s f)
    (define mp
      `(lib ,(apply ~a
                    #:separator "/"
                    (let ([l (map path-element->string 
                                  (explode-path f))])
                      (if single-collect
                          (if (eq? 'relative (car l))
                              (cons single-collect (cdr l))
                              (cons single-collect l))
                          l)))))
    (if (module-path? mp)
        (set-add s (collapse-module-path mp dummy))
        s))
  (parameterize ([current-directory dir])
    (for/fold ([s (set)]) ([f (in-directory)])
      (cond
       [(not (file-exists? f)) s]
       [else
        (define-values (base name dir?) (split-path f))
        (cond
         [(and (eq? 'relative base) (not single-collect)) s]
         [else
          (define bstr (path-element->bytes name))
          (cond
           [(or (equal? #"info.rkt" bstr)
                (equal? #"info.ss" bstr))
            ;; don't count "info.rkt" as a conflict, because
            ;; splices may need their own "info.rkt"s, and
            ;; `raco setup' can handle that
            s]
           [(regexp-match? #rx#"[.](?:rkt|ss)$" bstr)
            (try-path s f)]
           [(regexp-match? #rx#"_(?:rkt|ss)[.]zo$" (path-element->bytes name))
            (define-values (dir-base dir-name dir?) (split-path base))
            (cond
             [(eq? 'relative dir-base) s]
             [(equal? dir-name compiled)
              (define bstr2 (regexp-replace
                             #rx#"_(?:rkt|ss)[.]zo$"
                             (path-element->bytes name)
                             #".rkt"))
              (if (equal? #"info.rkt" bstr2)
                  s
                  (try-path s (build-path dir-base
                                          (bytes->path-element
                                           bstr2))))]
             [else s])]
           [else s])])]))))

(define (pkg-catalog-update-local #:catalog-file [catalog-file (db:current-pkg-catalog-file)]
                                  #:quiet? [quiet? #f]
                                  #:consult-packages? [consult-packages? #f])
  (parameterize ([db:current-pkg-catalog-file catalog-file])
    (define catalogs (pkg-config-catalogs))
    (db:set-catalogs! catalogs)

    (for ([catalog (in-list catalogs)])
      (parameterize ([current-pkg-catalogs (list (string->url catalog))])
        (define details (get-all-pkg-details-from-catalogs))
        ;; set packages:
        (db:set-pkgs! catalog (for/list ([(name ht) (in-hash details)])
                                (db:pkg name
                                        catalog
                                        (hash-ref ht 'author "")
                                        (hash-ref ht 'source "")
                                        (hash-ref ht 'checksum "")
                                        (hash-ref ht 'description ""))))
        ;; Add available module and dependency info:
        (for/list ([(name ht) (in-hash details)])
          (define checksum (hash-ref ht 'checksum ""))
          (define mods (hash-ref ht 'modules #f))
          (when mods
            (db:set-pkg-modules! name catalog checksum mods))
          (define deps (hash-ref ht 'dependencies #f))
          (when deps
            (db:set-pkg-dependencies! name catalog checksum deps)))
        (when consult-packages?
          ;; If module information isn't available for a package, download
          ;; the package to fill in that information:
          (define need-modules (db:get-pkgs-without-modules #:catalog catalog))
          (for ([(pkg) (in-list need-modules)])
            (define name (db:pkg-name pkg))
            (define ht (hash-ref details name))
            (define source (hash-ref ht 'source))
            (unless quiet?
              (printf "Downloading ~s\n" source))
            (define-values (checksum modules deps)
              (get-pkg-content (pkg-desc source
                                         #f 
                                         (hash-ref ht 'checksum #f) 
                                         #f)))
            (db:set-pkg-modules! name catalog checksum modules)
            (db:set-pkg-dependencies! name catalog checksum deps)))))))

(define (choose-catalog-file)
  (define default (db:current-pkg-catalog-file))
  (if (file-exists? default)
      default
      (let ([installation (build-path (find-lib-dir) "pkgs" (file-name-from-path default))])
        (if (file-exists? installation)
            installation
            default))))

(define (pkg-catalog-suggestions-for-module module-path
                                            #:catalog-file [catalog-file (choose-catalog-file)])
  (if (file-exists? catalog-file)
      (parameterize ([db:current-pkg-catalog-file catalog-file])
        (let* ([mod (collapse-module-path 
                     module-path
                     (lambda () (build-path (current-directory) "dummy.rkt")))]
               [pkgs (db:get-module-pkgs mod)]
               [more-pkgs (let ([rx:reader #rx"/lang/reader[.]rkt$"])
                            (if (and (pair? mod)
                                     (eq? (car mod) 'lib)
                                     (regexp-match rx:reader (cadr mod)))
                                (db:get-module-pkgs `(lib ,(regexp-replace rx:reader (cadr mod) "/main.rkt")))
                                null))])
          (sort (set->list
                 (list->set
                  (map db:pkg-name (append pkgs more-pkgs)))) 
                string<?)))
      null))
  
(define dep-behavior/c
  (or/c #f 'fail 'force 'search-ask 'search-auto))

(define package-scope/c
  (or/c 'installation 'user 'shared))

(provide
 with-pkg-lock
 with-pkg-lock/read-only
 (struct-out pkg-info)
 (struct-out sc-pkg-info)
 pkg-desc?
 (contract-out
  [current-pkg-scope
   (parameter/c package-scope/c)]
  [current-pkg-scope-version
   (parameter/c string?)]
  [current-pkg-error 
   (parameter/c procedure?)]
  [current-pkg-catalogs
   (parameter/c (or/c #f (listof url?)))]
  [pkg-directory
   (-> string? path-string?)]
  [path->pkg
   (-> path-string? (or/c #f string?))]
  [path->pkg+subpath
   (-> path-string? (values (or/c #f string?) (or/c #f 'same path?)))]
  [pkg-desc 
   (-> string? 
       (or/c #f 'file 'dir 'link 'file-url 'dir-url 'github 'name) 
       (or/c string? #f) 
       boolean?
       pkg-desc?)]
  [pkg-config
   (-> boolean? list?
       void?)]
  [pkg-create
   (->* ((or/c 'zip 'tgz 'plt 'MANIFEST)
         path-string?)
        (#:source (or/c 'dir 'name)
                  #:mode (or/c 'as-is 'source 'binary 'built)
                  #:quiet? boolean?
                  #:dest (or/c (and/c path-string? complete-path?) #f))
        void?)]
  [pkg-update
   (->* ((listof string?))
        (#:dep-behavior dep-behavior/c
                        #:all? boolean?
                        #:deps? boolean?
                        #:quiet? boolean?)
        (or/c #f (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-remove
   (->* ((listof string?))
        (#:auto? boolean?
                 #:force? boolean?
                 #:quiet? boolean?)
        (or/c #f (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-show
   (->* (string?)
        (#:directory? boolean?)
        void?)]
  [pkg-install
   (->* ((listof pkg-desc?))
        (#:dep-behavior dep-behavior/c
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:skip-installed? boolean?
                        #:quiet? boolean?)
        (or/c #f (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-catalog-show
   (->* ((listof string?))
        (#:all? boolean?
                #:only-names? boolean?
                #:modules? boolean?)
        void?)]
  [pkg-catalog-copy
   (->* ((listof path-string?) path-string?)
        (#:from-config? any/c
                        #:merge? boolean?
                        #:force? boolean?
                        #:override? boolean?)
        void?)]
  [default-pkg-scope
   (-> package-scope/c)]
  [installed-pkg-names
   (->* ()
        (#:scope (or/c #f package-scope/c))
        (listof string?))]
  [installed-pkg-table
   (->* ()
        (#:scope (or/c #f package-scope/c))
        (hash/c string? pkg-info?))]
  [pkg-stage (->* (pkg-desc?)
                  (#:checksum (or/c #f string?))
                  (values string?
                          path?
                          (or/c #f string?)
                          boolean?))]
  [pkg-config-catalogs
   (-> (listof string?))]
  [pkg-catalog-update-local
   (->* ()
        (#:catalog-file path-string?
         #:quiet? boolean?
         #:consult-packages? boolean?)
        void?)]
  [pkg-catalog-suggestions-for-module
   (->* (module-path?)
        (#:catalog-file path-string?)
        (listof string?))]
  [get-all-pkg-names-from-catalogs
   (-> (listof string?))]
  [get-all-pkg-details-from-catalogs
   (-> (hash/c string? (hash/c symbol? any/c)))]
  [get-pkg-details-from-catalogs
   (-> string?
       (or/c #f (hash/c symbol? any/c)))]
  [get-pkg-content
   (->* (pkg-desc?)
        (#:extract-info (-> (or/c #f
                                  ((symbol?) ((-> any)) . ->* . any))
                            any/c))
        (values (or/c #f string?)
                (listof module-path?)
                any/c))]
  [pkg-single-collection
   (->* (path-string?)
        (#:namespace namespace?)
        (or/c #f string?))]))
