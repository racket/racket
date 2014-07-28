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
         setup/collection-name
         setup/matching-platform
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
         file/cache
         setup/getinfo
         setup/dirs
         racket/format
         version/utils
         syntax/modcollapse
         syntax/modread
         compiler/compilation-path
         "name.rkt"
         "util.rkt"
         "strip.rkt"
         "path.rkt"
         (prefix-in db: "db.rkt"))

(define current-pkg-scope
  (make-parameter 'user (lambda (p)
                          (if (path? p)
                              (simple-form-path p)
                              p))))
(define current-pkg-scope-version
  (make-parameter (get-installation-name)))
(define current-pkg-lookup-version
  (make-parameter (version)))
(define current-pkg-error
  (make-parameter (lambda args (apply error 'pkg args))))
(define current-no-pkg-db
  (make-parameter #f))
(define current-pkg-catalogs
  (make-parameter #f))

(define current-pkg-download-cache-dir
  (make-parameter #f))
(define current-pkg-download-cache-max-files
  (make-parameter #f))
(define current-pkg-download-cache-max-bytes
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

(define (printf/flush fmt . args)
  ;; For status reporting, flush immediately after printing
  (apply printf fmt args)
  (flush-output))

(struct pkg-desc (source type name checksum auto?))
(define (pkg-desc=? a b)
  (define (->list a)
    (list (pkg-desc-source a)
          (pkg-desc-type a)
          (pkg-desc-name a)
          (pkg-desc-checksum a)
          (pkg-desc-auto? a)))
  (equal? (->list a) (->list b)))

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

(define (download-file! url file checksum
                        #:download-printf [download-printf #f]
                        #:use-cache? [use-cache? #t]
                        #:fail-okay? [fail-okay? #f])
  (with-handlers ([exn:fail?
                   (λ (x)
                     (unless fail-okay?
                       (raise x)))])
    (make-parent-directory* file)
    (log-pkg-debug "\t\tDownloading ~a to ~a" (url->string url) file)
    (define (download!)
      (when download-printf
        (download-printf "Downloading ~a\n" (url->string url)))
      (call-with-output-file file
        (λ (op)
          (call/input-url+200
           url
           (λ (ip) (copy-port ip op))
           #:failure
           (lambda (reply-s)
             (pkg-error (~a "error downloading package\n"
                            "  URL: ~a\n"
                            "  server response: ~a")
                        (url->string url)
                        (read-line (open-input-string reply-s))))))))
    (cond
     [(and checksum use-cache?)
      (cache-file file
                  (list (url->string url) checksum)
                  (get-download-cache-dir)
                  download!
                  #:log-error-string (lambda (s) (log-pkg-error s))
                  #:log-debug-string (lambda (s) (log-pkg-debug s))
                  #:notify-cache-use (lambda (s)
                                       (when download-printf
                                         (download-printf "Using ~a for ~a\n"
                                                          s
                                                          (url->string url))))
                  #:max-cache-files (get-download-cache-max-files)
                  #:max-cache-size (get-download-cache-max-bytes))]
     [else (download!)])))

(define (clean-cache pkg-url checksum)
  (when pkg-url
    ;; Something failed after download, so remove cached file (if any):
    (with-handlers ([exn:fail? void]) ; any error is logged already
      (cache-remove (list (url->string pkg-url) checksum)
                    (get-download-cache-dir)
                    #:log-error-string (lambda (s) (log-pkg-error s))
                    #:log-debug-string (lambda (s) (log-pkg-debug s))))))

(define (pkg-dir config?)
  (define scope (current-pkg-scope))
  (if (and config?
           (eq? scope 'installation))
      (find-config-dir)
      (get-pkgs-dir scope (current-pkg-scope-version))))
(define (pkg-config-file)
  (build-path (pkg-dir #t) "config.rktd"))
(define (pkg-db-file)
  (build-path (pkg-dir #f) "pkgs.rktd"))
(define (pkg-installed-dir)
  (pkg-dir #f))
(define (pkg-lock-file)
  (make-lock-file-name (pkg-db-file)))

(define (get-download-cache-dir)
  (or (current-pkg-download-cache-dir)
      (read-pkg-cfg/def 'download-cache-dir)))
(define (get-download-cache-max-files)
  (or (current-pkg-download-cache-max-files)
      (read-pkg-cfg/def 'download-cache-max-files)))
(define (get-download-cache-max-bytes)
  (or (current-pkg-download-cache-max-bytes)
      (read-pkg-cfg/def 'download-cache-max-bytes)))

(define (make-metadata-namespace)
  (make-base-empty-namespace))

(define (get-pkg-info pkg-dir metadata-ns)
  (with-handlers ([exn:fail? (λ (x)
                                (log-exn x "getting info")
                                #f)])
    (get-info/full pkg-dir 
                   #:namespace metadata-ns
                   #:bootstrap? #t)))

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

(define (get-all-implies metadata-ns pkg-dir deps)
  (get-metadata metadata-ns pkg-dir 
                'implies (lambda () empty)
                #:checker (lambda (l)
                            (unless (null? l)
                              (define deps-set (list->set
                                                (map dependency->name deps)))
                              (unless (and (list? l)
                                           (andmap (lambda (v)
                                                     (or (string? v)
                                                         (eq? v 'core)))
                                                   l))
                                (pkg-error (~a "invalid `implies' specification\n"
                                               "  specification: ~e")
                                           l))
                              (unless (andmap (lambda (i)
                                                (or (eq? i 'core)
                                                    (set-member? deps-set i)))
                                              l)
                                (pkg-error (~a "`implies' is not a subset of dependencies\n"
                                               "  specification: ~e")
                                           l))))))

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
  (or (not p) (matching-platform? p)))

(define pkg-lock-held (make-parameter #f))
(define pkg-lock-scope (make-parameter #f))

;; Call `t' with lock held for the current scope. The intent is that
;; `t' reads and writes package information in the curent scope. It
;; may also *read* package information for wider package scopes
;; without a further lock --- which is questionable, but modification
;; of a shared scope while others are running can create trouble,
;; anyway.
(define (with-pkg-lock* read-only? t)
  (define mode (if read-only? 'shared 'exclusive))
  (define held-mode (pkg-lock-held))
  (define now-scope (current-pkg-scope))
  (define held-scope (pkg-lock-scope))
  (when (and held-scope
             (not (eq? held-scope now-scope)))
    (pkg-error "lock mismatch\n  held scope: ~a\n  requested scope: ~a"
               held-scope
               now-scope))
  (if (or (eq? mode held-mode)
          (eq? 'exclusive held-mode))
      (t)
      (let ([d (pkg-dir #f)])
        (unless read-only? (make-directory* d))
        (if (directory-exists? d)
            ;; If the directory exists, assume that a lock file is
            ;; available or creatable:
            (call-with-file-lock/timeout
             #f 
             mode
             (lambda ()
               (parameterize ([pkg-lock-held mode]
                              [pkg-lock-scope now-scope]
                              [current-no-pkg-db #f])
                 (t)))
             (λ () (pkg-error  (~a "could not acquire package lock\n"
                                   "  lock file: ~a")
                               (pkg-lock-file)))
             #:lock-file (pkg-lock-file))
            ;; Directory does not exist; we must be in read-only mode.
            ;; Run `t' under the claim that no database is available
            ;; (in case the database is created concurrently):
            (parameterize ([current-no-pkg-db now-scope])
              (parameterize ([pkg-lock-held mode])
                (t)))))))
(define-syntax-rule (with-pkg-lock e ...)
  (with-pkg-lock* #f (λ () e ...)))
(define-syntax-rule (with-pkg-lock/read-only e ...)
  (with-pkg-lock* #t (λ () e ...)))

(define (maybe-append lists)
  (and (for/and ([v (in-list lists)]) (not (eq? v 'all)))
       (apply append lists)))

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

;; Add current package version to a URL:
(define (add-version-query addr/no-query)
  (struct-copy url addr/no-query
               [query (append
                       (url-query addr/no-query)
                       (list
                        (cons 'version (current-pkg-lookup-version))))]))

;; Take a package-info hash table and lift any version-specific
;; information in 'versions.
(define (select-info-version ht)
  (and ht
       (let ([v (hash-ref ht 'versions #f)])
         (cond
          [(hash? v)
           (or (for/or ([vers (in-list (list (current-pkg-lookup-version)
                                             'default))])
                 (define ht2 (hash-ref v vers #f))
                 (and ht2
                      ;; Override fields of `ht' with values from `ht2':
                      (for/fold ([ht ht]) ([(k v) (in-hash ht2)])
                        (hash-set ht k v))))
               ;; Keep ht as-is:
               ht)]
          [else ht]))))

;; If the 'source field in `ht` is a relative path, treat
;; it as relative to `i` and make it absolute:
(define (source->absolute-source i ht)
  (cond
   [ht
    (define s (hash-ref ht 'source #f))
    (define new-ht
      (cond
       [s
        ;; If `s' is a relative URL, then we rely on the pun
        ;; that it will parse as a relative path.
        (define-values (name type) (package-source->name+type s #f))
        (cond
         [(and (or (eq? type 'dir) (eq? type 'file))
               (not (regexp-match? #rx"^file://" s))
               (relative-path? s))
          (define i-for-combine
            (cond
             [(equal? "file" (url-scheme i))
              (define i-path (url->path i))
              (if (db-path? i-path)
                  i
                  ;; Make sure we interpret `i' as a directory when
                  ;; adding a relative path:
                  (path->url (path->directory-path (url->path i))))]
             [else i]))
          (define full-url
            (url->string
             (combine-url/relative i-for-combine s)))
          (hash-set ht 'source full-url)]
         [else ht])]
       [else ht]))
    (let ([v (hash-ref new-ht 'versions #f)])
      (if v
          ;; Adjust version-specific sources:
          (hash-set new-ht 'versions
                    (for/hash ([(k ht) (in-hash v)])
                      (values k (source->absolute-source i ht))))
          ;; No further adjustments:
          new-ht))]
   [else #f]))

;; Make sources in `ht` relative to `dir`, when possible:
(define (source->relative-source dir ht)
  (define s (hash-ref ht 'source #f))
  (define new-ht
    (cond
     [s
      (define-values (name type) (package-source->name+type s #f))
      (cond
       [(or (eq? type 'dir) (eq? type 'file))
        (hash-set ht
                  'source
                  (relative-path->relative-url-string
                   (find-relative-path
                    dir
                    (package-source->path s type))))]
       [else ht])]
     [else ht]))
  (let ([v (hash-ref new-ht 'versions #f)])
    (if v
        ;; Adjust version-specific sources:
        (hash-set new-ht 'versions
                  (for/hash ([(k ht) (in-hash new-ht)])
                    (values k (source->relative-source dir ht))))
        ;; No further adjustments:
        new-ht)))

(define (package-catalog-lookup pkg details? download-printf)
  (or
   (for/or ([i (in-list (pkg-catalogs))])
     (if download-printf
         (download-printf "Resolving ~s via ~a\n" pkg (url->string i))
         (log-pkg-debug "consulting catalog ~a" (url->string i)))
     (source->absolute-source
      i
      (select-info-version
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
               (call-with-input-file* pkg-path read)))))))
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
            'checksum (db:pkg-checksum pkg))))

(define (remote-package-checksum pkg download-printf pkg-name)
  (match pkg
    [`(catalog ,pkg-name)
     (hash-ref (package-catalog-lookup pkg-name #f download-printf) 'checksum)]
    [`(url ,pkg-url-str)
     (package-url->checksum pkg-url-str 
                            #:download-printf download-printf
                            #:pkg-name pkg-name)]))

(define (checksum-for-pkg-source pkg-source type pkg-name given-checksum download-printf)
  (case type
    [(file-url dir-url github)
     (or given-checksum
	 (remote-package-checksum `(url ,pkg-source) download-printf pkg-name))]
    [(file)
     (define checksum-pth (format "~a.CHECKSUM" pkg-source))
     (or (and (file-exists? checksum-pth)
	      (file->string checksum-pth))
	 (and (file-exists? pkg-source)
	      (call-with-input-file* pkg-source sha1)))]
    [else given-checksum]))

(define (write-file-hash! file new-db)
  (unless (eq? (pkg-lock-held) 'exclusive)
    (pkg-error "attempt to write package database without write lock"))
  (make-parent-directory* file)
  (call-with-atomic-output-file
   file
   (λ (o tmp-path) (write new-db o) (newline o))))

(define (read-pkg-db)
  (unless (pkg-lock-held)
    (pkg-error "attempt to read package database without lock"))
  (define scope (current-pkg-scope))
  (if (eq? (current-no-pkg-db) scope)
      #hash()
      (read-pkgs-db scope (current-pkg-scope-version))))

;; read all packages in this scope or wider
(define (merge-pkg-dbs [scope (current-pkg-scope)])
  (define (merge-next-pkg-dbs scope)
    (parameterize ([current-pkg-scope scope])
      (merge-pkg-dbs scope)))
  (if (path? scope)
      (read-pkg-db)
      (case scope
        [(installation)
         (for*/hash ([dir (in-list (get-pkgs-search-dirs))]
                     [(k v) (read-pkgs-db dir)])
           (values k v))]
        [(user)
         (define db (read-pkgs-db 'user (current-pkg-scope-version)))
         (for/fold ([ht (merge-next-pkg-dbs 'installation)]) ([(k v) (in-hash db)])
           (hash-set ht k v))])))    

;; Finds the scope, in which `pkg-name' is installed; returns 'dir,
;; 'installation, a path, or #f (where #f means "not installed").  If
;; `next?' is true, search only scopes wider than the current one.
(define (find-pkg-installation-scope pkg-name #:next? [next? #f])
  (case (current-pkg-scope)
    [(user)
     (or (and (not next?)
              (hash-ref (read-pkg-db) pkg-name #f)
              'user)
         (parameterize ([current-pkg-scope 'installation])
           (find-pkg-installation-scope pkg-name)))]
    [(installation)
     (or (and (not next?)
              (hash-ref (read-pkg-db) pkg-name #f)
              'installation)
         (for/or ([dir (in-list (get-pkgs-search-dirs))])
           (and (hash-ref (read-pkgs-db dir) pkg-name #f)
                dir)))]
    [else
     (and (not next?)
          (and (hash-ref (read-pkgs-db (current-pkg-scope)) pkg-name #f)
               (current-pkg-scope)))]))

(define (package-info pkg-name [fail? #t] #:db [given-db #f])
  (define db (or given-db (read-pkg-db)))
  (define pi (hash-ref db pkg-name #f))
  (cond
    [pi
     pi]
    [(not fail?)
     #f]
    [else
     (pkg-not-installed pkg-name db)]))

;; return the current scope as a string
(define (current-scope->string)
  (define scope (current-pkg-scope))
  (cond
   [(path? scope) (path->string scope)]
   [else (symbol->string scope)]))

;; prints an error for packages that are not installed
;; pkg-name db -> void
(define (pkg-not-installed pkg-name db)
  ;; This may read narrower package scopes without holding the
  ;; lock, but maybe that's ok for mere error reporting:
  (define s (parameterize ([current-pkg-scope 'user])
              (find-pkg-installation-scope pkg-name)))

  (define not-installed-msg
   (cond [s "package installed in a different scope"]
         [else "package not currently installed"]))

  (apply pkg-error (~a not-installed-msg
                       "\n  package: ~a"
                       "\n  current scope: ~a"
                       (if s
                           "\n  installed in scope: ~a"
                           "")
                       ;; Probably too much information:
                       #;
                       "\n  packages in current scope:~a")
         (append
          (list
           pkg-name
           (current-scope->string))
          (if s (list s) null)
          #;
          (list
           (format-list (hash-keys db))))))
         
(define (update-pkg-db! pkg-name info)
  (write-file-hash!
   (pkg-db-file)
   (hash-set (read-pkg-db) pkg-name info)))
(define (remove-from-pkg-db! pkg-name)
  (write-file-hash!
   (pkg-db-file)
   (hash-remove (read-pkg-db) pkg-name)))

(define (read-pkg-cfg/def k)
  ;; Lock is held for the current scope, but if
  ;; the key is not found in the current scope,
  ;; get the next scope's lock and try there,
  ;; etc.
  (define (get-default)
    (match k
      ['catalogs
       (list "http://pkgs.racket-lang.org"
             "http://planet-compats.racket-lang.org")]
      ['default-scope "user"]
      ['installation-name (version)]
      ['download-cache-dir (build-path (find-system-path 'addon-dir)
                                       "download-cache")]
      ['download-cache-max-files 1024]
      ['download-cache-max-bytes (* 64 1024 1024)]
      [_ #f]))
  (define c (read-pkg-file-hash (pkg-config-file)))
  (define v (hash-ref c k 'none))
  (cond
   [(eq? v 'none)
    ;; Default from enclosing scope or hard-wired default:
    (define s (current-pkg-scope))
    (if (eq? s 'installation)
        ;; Hard-wided:
        (get-default)
        ;; Enclosing:
        (parameterize ([current-pkg-scope 'installation])
          (read-pkg-cfg/def k)))]
   [else
    (match k
      ['catalogs
       (if (member #f v)
           ;; Replace #f with default URLs:
           (apply append (for/list ([i (in-list v)])
                           (if (not i)
                               (get-default)
                               (list i))))
           v)]
      [_ v])]))

(define (update-pkg-cfg! key val)
  (define f (pkg-config-file))
  (write-file-hash! 
   f
   (hash-set (read-pkg-file-hash f) key val)))

(define (default-pkg-scope)
  (match (default-pkg-scope-as-string)
    ["installation" 'installation]
    [else 'user]))
(define (default-pkg-scope-as-string)
  (read-pkg-cfg/def 'default-scope))

(define (pkg-config-catalogs)
  (with-pkg-lock/read-only
   (read-pkg-cfg/def 'catalogs)))

(define (pkg-catalogs)
  (or (current-pkg-catalogs)
      (map string->url (read-pkg-cfg/def 'catalogs))))

(struct install-info (name orig-pkg directory clean? checksum module-paths additional-installs))

(define (update-install-info-orig-pkg if op)
  (struct-copy install-info if
               [orig-pkg op]))
(define (update-install-info-checksum if op)
  (struct-copy install-info if
               [checksum op]))

(define (scope->links-file scope)
  (and (path? scope)
       (build-path scope "links.rktd")))

(define (get-scope-list)
  ;; Get a list of scopes suitable for searches with respect to
  ;; the current scope
  (define current-scope (current-pkg-scope))
  (if (path? current-scope)
      (list current-scope)
      (member current-scope
              (append '(user)
                      (let ([main (find-pkgs-dir)])
                        (for/list ([d (get-pkgs-search-dirs)])
                          (if (equal? d main)
                              'installation
                              d)))))))

(define (pkg-directory pkg-name)
  ;; Warning: takes locks individually.
  (pkg-directory** pkg-name 
                   (lambda (f)
                     (with-pkg-lock/read-only
                      (f)))))

(define (pkg-directory** pkg-name [call-with-pkg-lock (lambda (f) (f))])
  (for/or ([scope (in-list (get-scope-list))])
    (parameterize ([current-pkg-scope scope])
      (call-with-pkg-lock
       (lambda ()
         (pkg-directory* pkg-name))))))

(define (pkg-directory* pkg-name #:db [db #f])
  (define info (package-info pkg-name #f #:db db))
  (and info
       (let ()
         (match-define (pkg-info orig-pkg checksum _) info)
         (match orig-pkg
           [`(,(or 'link 'static-link) ,orig-pkg-dir)
            (path->complete-path orig-pkg-dir (pkg-installed-dir))]
           [_
            (build-path (pkg-installed-dir) 
                        (or (cond
                             [(pkg-info/alt? info)
                              (pkg-info/alt-dir-name info)]
                             [(sc-pkg-info/alt? info)
                              (sc-pkg-info/alt-dir-name info)]
                             [else #f])
                            pkg-name))]))))

(define (make-pkg-info orig-pkg checksum auto? single-collect alt-dir-name)
  ;; Picks the right structure subtype
  (if single-collect
      (if alt-dir-name
          (sc-pkg-info/alt orig-pkg checksum auto? single-collect alt-dir-name)
          (sc-pkg-info orig-pkg checksum auto? single-collect))
      (if alt-dir-name
          (pkg-info/alt orig-pkg checksum auto? alt-dir-name)
          (pkg-info orig-pkg checksum auto?))))

(define (update-auto this-pkg-info auto?)
  (match-define (pkg-info orig-pkg checksum _) this-pkg-info)
  (make-pkg-info orig-pkg checksum auto?
                 (and (sc-pkg-info? this-pkg-info)
                      (sc-pkg-info-collect this-pkg-info))
                 (or (and (sc-pkg-info/alt? this-pkg-info)
                          (sc-pkg-info/alt-dir-name this-pkg-info))
                     (and (pkg-info/alt? this-pkg-info)
                          (pkg-info/alt-dir-name this-pkg-info)))))

(define (demote-packages quiet? pkg-names)
  (define db (read-pkg-db))
  (for ([pkg-name (in-list pkg-names)])
    (define pi (package-info pkg-name #:db db))
    (unless (pkg-info-auto? pi)
      (unless quiet?
        (printf/flush "Demoting ~a to auto-installed\n" pkg-name))
      (update-pkg-db! pkg-name (update-auto pi #t)))))

(define ((remove-package quiet?) pkg-name)
  (unless quiet?
    (printf/flush "Removing ~a\n" pkg-name))
  (define db (read-pkg-db))
  (define pi (package-info pkg-name #:db db))
  (match-define (pkg-info orig-pkg checksum _) pi)
  (define pkg-dir (pkg-directory* pkg-name #:db db))
  (remove-from-pkg-db! pkg-name)
  (define scope (current-pkg-scope))
  (define user? (not (or (eq? scope 'installation)
                         (path? scope))))
  (match orig-pkg
    [`(,(or 'link 'static-link) ,_)
     (links pkg-dir
            #:remove? #t
            #:user? user?
            #:file (scope->links-file scope)
            #:root? (not (sc-pkg-info? pi)))]
    [_
     (links pkg-dir
            #:remove? #t
            #:user? user?
            #:file (scope->links-file scope)
            #:root? (not (sc-pkg-info? pi)))
     (delete-directory/files pkg-dir)]))

      

(define (pkg-remove given-pkgs
                    #:demote? [demote? #f]
                    #:force? [force? #f]
                    #:auto? [auto? #f]
                    #:quiet? [quiet? #f]
                    #:from-command-line? [from-command-line? #f])
  (define db (read-pkg-db))
  (define all-pkgs
    (hash-keys db))
  (define all-pkgs-set
    (list->set all-pkgs))
  (define metadata-ns (make-metadata-namespace))
  (define in-pkgs (remove-duplicates given-pkgs))
  (define remove-pkgs
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
               (append-map (package-dependencies metadata-ns db #t)
                           (set->list keep))))
            (define still-drop (set-subtract drop deps))
            (define delta (set-subtract drop still-drop))
            (if (set-empty? delta)
                (set->list drop)
                (loop still-drop
                      (set-union keep delta)))))
        ;; just given pkgs:
        (if demote?
            null
            in-pkgs)))
  (define setup-collects
    (get-setup-collects remove-pkgs
                        db
                        metadata-ns))
  (unless (or force? demote?)
    ;; Check dependencies on `in-pkgs' (not `pkgs', which has already
    ;; been filtered to remove package with dependencies if `auto?' is
    ;; true).
    (define pkgs-set (list->set in-pkgs))
    (define remaining-pkg-db-set
      (set-subtract all-pkgs-set
                    (if auto?
                        (list->set remove-pkgs)
                        pkgs-set)))
    (define deps-to-be-removed
      (set-intersect
       pkgs-set
       (list->set
        (append-map (package-dependencies metadata-ns db #t)
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
                                 (member p ((package-dependencies metadata-ns db #t) dp)))
                               (set->list
                                remaining-pkg-db-set)))
                     (~a p " (required by: " ds ")"))
                   (set->list deps-to-be-removed))))))

  (when demote?
    ;; Demote any package that is not going to be removed:
    (demote-packages
     quiet?
     (set->list (set-subtract (list->set in-pkgs)
                              (list->set remove-pkgs)))))

  (for-each (remove-package quiet?)
            remove-pkgs)

  (cond
   [(or (null? remove-pkgs) demote?)
    ;; Did nothing, so no setup:
    'skip]
   [else
    ;; setup only collections that still exist:
    (and setup-collects
         (for/list ([c (in-list setup-collects)]
                    #:when (apply collection-path
                                  (if (path-string? c) (list c) c)
                                  #:fail (lambda (s) #f)))
           c))]))

(define (complain-about-source s reason)
  (pkg-error (~a "invalid package source;\n"
                 " ~a\n"
                 "  given: ~a")
             reason
             s))

(define (check-checksum given-checksum checksum what pkg-src cached-url)
  (when (and given-checksum
             checksum
             (not (equal? given-checksum checksum)))
    (clean-cache cached-url checksum)
    (pkg-error (~a "~a checksum on package\n"
                   "  package source: ~a\n"
                   "  expected: ~e\n"
                   "  got: ~e")
               what
               pkg-src
               given-checksum
               checksum)))

(define (drop-redundant-files pkg-dir)
  ;; Ad hoc space-saving rule: for an installation-wide package, remove
  ;; any redundant "COPYING.txt" or "COPYING_LESSER.txt" files.
  (when (and (eq? 'installation (current-pkg-scope))
             (find-share-dir))
    (for ([i (in-list '("COPYING.txt" "COPYING_LESSER.txt"))])
      (define pkg-file (build-path pkg-dir i))
      (define share-file (build-path (find-share-dir) i))
      (when (and (file-exists? pkg-file)
                 (file-exists? share-file)
                 (equal? (file->bytes pkg-file)
                         (file->bytes share-file)))
        ;; This file would be redundant, so drop it
        (delete-file pkg-file)))))

;; Downloads a package (if needed) and unpacks it (if needed) into a  
;; temporary directory.
(define (stage-package/info pkg
                            given-type
                            given-pkg-name
                            #:given-checksum [given-checksum #f]
                            #:cached-url [cached-url #f]
                            #:use-cache? use-cache?
                            check-sums?
                            download-printf
                            metadata-ns
                            #:strip [strip-mode #f]
                            #:in-place? [in-place? #f]
                            #:in-place-clean? [in-place-clean? #f]
                            #:link-dirs? [link-dirs? #f])
  (define-values (inferred-pkg-name type) 
    (if (path? pkg)
        (package-source->name+type (path->string pkg)
                                   (or given-type
                                       (if (directory-exists? pkg)
                                           (if link-dirs?
                                               'link
                                               'dir)
                                           'file))
                                   #:must-infer-name? (not given-pkg-name)
                                   #:complain complain-about-source)
        (package-source->name+type pkg given-type
                                   #:link-dirs? link-dirs?
                                   #:must-infer-name? (not given-pkg-name)
                                   #:complain complain-about-source)))
  (define pkg-name (or given-pkg-name inferred-pkg-name))
  (when (and type (not pkg-name))
    (pkg-error (~a "could not infer package name from source\n"
                   "  source: ~a")
               pkg))
  (cond
   [(and (eq? type 'github)
         (not (regexp-match? #rx"^git(?:hub)?://" pkg)))
    ;; Add "git://github.com/"
    (stage-package/info (string-append "git://github.com/" pkg) type 
                        pkg-name 
                        #:given-checksum given-checksum
                        #:use-cache? use-cache?
                        check-sums? download-printf
                        metadata-ns
                        #:strip strip-mode)]
   [(or (eq? type 'file-url) (eq? type 'dir-url) (eq? type 'github))
    (define pkg-url (string->url pkg))
    (define scheme (url-scheme pkg-url))

    (define orig-pkg `(url ,pkg))
    (define found-checksum
      ;; If a checksum is given, use that. In the case of a non-github
      ;; source, we could try to get the checksum from the source, and
      ;; then check whether it matches the expected one, but we choose
      ;; to avoid an extra trip to the server.
      (or given-checksum
          (remote-package-checksum orig-pkg download-printf pkg-name)))
    (when check-sums?
      (check-checksum given-checksum found-checksum "unexpected" pkg #f))
    (define checksum (or found-checksum given-checksum))
    (define downloaded-info
       (match type
         ['github
          (unless checksum
            (pkg-error 
             (~a "could not find checksum for GitHub package source, which implies it doesn't exist\n"
                 "  source: ~a")
             pkg))
          (when (equal? checksum "")
            (pkg-error 
             (~a "cannot use empty checksum for GitHub package source\n"
                 "  source: ~a")
             pkg))
          (match-define (list* user repo branch path)
                        (split-github-url pkg-url))
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

          (dynamic-wind
              void
              (λ ()
                 (download-file! new-url tmp.tgz checksum
                                 #:use-cache? use-cache?
                                 #:download-printf download-printf)
                 (define staged? #f)
                 (dynamic-wind
                     void
                     (λ ()
                        (untar tmp.tgz tmp-dir #:strip-components 1)

                        (unless (null? path)
                          (unless (directory-exists? (apply build-path tmp-dir path))
                            (pkg-error
                             (~a "specified directory is not in GitHub respository archive\n"
                                 "  path: ~a"
                                 (apply build-path path))))
                          (lift-directory-content tmp-dir path))

                        (begin0
                         (stage-package/info tmp-dir
                                             'dir
                                             pkg-name
                                             #:given-checksum checksum
                                             #:cached-url new-url
                                             #:use-cache? use-cache?
                                             check-sums?
                                             download-printf
                                             metadata-ns
                                             #:strip strip-mode
                                             #:in-place? #t
                                             #:in-place-clean? #t)
                         (set! staged? #t)))
                     (λ ()
                       (when (and use-cache? (not staged?))
                         (clean-cache new-url checksum))
                       (unless staged?
                         (delete-directory/files tmp-dir)))))
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
                         (download-printf "Cloning remote directory ~a\n"
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
                                           (path-like f)
                                           #f
                                           #:use-cache? use-cache?))))]
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
                        (download-file! pkg-url package-path checksum
                                        #:use-cache? use-cache?
                                        #:download-printf download-printf)))]))
          (define staged? #f)
          (dynamic-wind
              void
              (λ ()
                 (download-package!)
                 (log-pkg-debug "\tDownloading done, installing ~a as ~a"
                                package-path pkg-name)
                 (begin0
                  (stage-package/info package-path
                                      download-type
                                      pkg-name
                                      #:given-checksum checksum
                                      #:cached-url pkg-url
                                      #:use-cache? use-cache?
                                      check-sums?
                                      download-printf
                                      metadata-ns
                                      #:strip strip-mode)
                  (set! staged? #t)))
              (λ ()
                 (when (or (file-exists? package-path)
                           (directory-exists? package-path))
                   (when (and use-cache? (not staged?))
                     (clean-cache pkg-url checksum))
                   (delete-directory/files package-path))))]))
    (define info (update-install-info-orig-pkg downloaded-info
                                               orig-pkg))
    (when (and check-sums?
               (install-info-checksum info)
               (not checksum))
      (pkg-error (~a "remote package had no checksum\n"
                     "  package: ~a")
                 pkg))
    (when check-sums?
      (check-checksum checksum (install-info-checksum info)
                      "mismatched"
                      pkg
                      (and use-cache? cached-url)))
    (update-install-info-checksum
     info
     checksum)]
   [(eq? type 'file)
    (define pkg-path (if (path? pkg)
                         pkg
                         (package-source->path pkg type)))
    (unless (file-exists? pkg-path)
      (pkg-error "no such file\n  path: ~a" pkg-path))
    (define checksum-pth (format "~a.CHECKSUM" pkg-path))
    (define expected-checksum
      (and (file-exists? checksum-pth)
           check-sums?
           (file->string checksum-pth)))
    (check-checksum given-checksum expected-checksum "unexpected" pkg-path #f)
    (define actual-checksum
      (with-input-from-file pkg-path
        (λ ()
           (sha1 (current-input-port)))))
    (check-checksum expected-checksum actual-checksum "mismatched" pkg-path
                    (and use-cache? cached-url))
    (define checksum
      actual-checksum)
    (define pkg-format (filename-extension pkg-path))
    (define pkg-dir
      (make-temporary-file (string-append "~a-" pkg-name)
                           'directory))
    (define staged? #f)
    (dynamic-wind
        void
        (λ ()
           (make-directory* pkg-dir)

           (match pkg-format
             [#"tgz"
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"tar"
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"gz" ; assuming .tar.gz
              (untar pkg-path pkg-dir)
              (remove-extra-directory-layer pkg-dir)]
             [#"zip"
              (unzip pkg-path (make-filesystem-entry-reader #:dest pkg-dir)
                     #:preserve-timestamps? #t
                     #:utc-timestamps? #t)
              (remove-extra-directory-layer pkg-dir)]
             [#"plt"
              (make-directory* pkg-dir)
              (unpack pkg-path pkg-dir
                      (lambda (x) (log-pkg-debug "~a" x))
                      (lambda () pkg-dir)
                      #f
                      (lambda (auto-dir main-dir file) pkg-dir))
              (define info-path (build-path pkg-dir "info.rkt"))
              (unless (file-exists? info-path)
                ;; Add in "info.rkt" file to make it multi-collection,
                ;; since a ".plt" archive is never single-collection. This
                ;; is needed for supporting old ".plt" archives as packages.
                (call-with-output-file info-path
                  (lambda (o)
                    (fprintf o "#lang setup/infotab\n")
                    (write '(define collection 'multi) o)
                    (newline o))))]
             [x
              (pkg-error "invalid package format\n  given: ~a" x)])

           (begin0
            (update-install-info-checksum
             (update-install-info-orig-pkg
              (stage-package/info pkg-dir
                                  'dir
                                  pkg-name
                                  #:given-checksum checksum
                                  #:cached-url cached-url
                                  #:use-cache? use-cache?
                                  check-sums?
                                  download-printf
                                  metadata-ns
                                  #:strip strip-mode
                                  #:in-place? (not strip-mode)
                                  #:in-place-clean? #t)
              `(file ,(simple-form-path* pkg-path)))
             checksum)
            (unless strip-mode
              (set! staged? #t))))
        (λ ()
           (unless staged?
             (delete-directory/files pkg-dir))))]
   [(or (eq? type 'dir)
        (eq? type 'link)
        (eq? type 'static-link))
    (define pkg-path (if (path? pkg)
                         pkg
                         (package-source->path pkg type)))
    (unless (directory-exists? pkg-path)
      (pkg-error "no such directory\n  path: ~a" pkg-path))
    (let ([pkg-path (directory-path-no-slash pkg-path)])
      (cond
       [(or (eq? type 'link)
            (eq? type 'static-link))
        (install-info pkg-name
                      `(,type ,(path->string
                                (find-relative-path (pkg-installed-dir)
                                                    (simple-form-path pkg-path)
                                                    #:more-than-root? #t)))
                      pkg-path
                      #f
                      given-checksum ; if a checksum is provided, just use it
                      (directory->module-paths pkg pkg-name metadata-ns)
                      (directory->additional-installs pkg pkg-name metadata-ns))]
       [else
        (define pkg-dir
          (if in-place?
              (if strip-mode
                  (pkg-error "cannot strip directory in place")
                  pkg-path)
              (let ([pkg-dir (make-temporary-file "pkg~a" 'directory)])
                (delete-directory pkg-dir)
                (if strip-mode
                    (begin
                      (make-directory* pkg-dir)
                      (generate-stripped-directory strip-mode pkg pkg-dir))
                    (begin
                      (make-parent-directory* pkg-dir)
                      (copy-directory/files pkg-path pkg-dir #:keep-modify-seconds? #t)))
                pkg-dir)))
        (when (or (not in-place?)
                  in-place-clean?)
          (drop-redundant-files pkg-dir))
        (install-info pkg-name
                      `(dir ,(simple-form-path* pkg-path))
                      pkg-dir
                      (or (not in-place?) in-place-clean?) 
                      given-checksum ; if a checksum is provided, just use it
                      (directory->module-paths pkg-dir pkg-name metadata-ns)
                      (directory->additional-installs pkg-dir pkg-name metadata-ns))]))]
   [(eq? type 'name)
    (define catalog-info (package-catalog-lookup pkg #f download-printf))
    (log-pkg-debug "catalog response: ~s" catalog-info)
    (define source (hash-ref catalog-info 'source))
    (define checksum (hash-ref catalog-info 'checksum))
    (define info (stage-package/info source
                                     #f
                                     pkg-name
                                     #:given-checksum checksum
                                     #:use-cache? use-cache?
                                     check-sums?
                                     download-printf
                                     metadata-ns
                                     #:strip strip-mode))
    (when check-sums?
      (check-checksum given-checksum checksum "unexpected" pkg #f)
      (check-checksum checksum (install-info-checksum info) "incorrect" pkg #f))
    (update-install-info-orig-pkg
     (update-install-info-checksum
      info
      checksum)
     `(catalog ,pkg))]
   [else
    (pkg-error "cannot infer package source type\n  source: ~a" pkg)]))

(define (pkg-stage desc
                   #:namespace [metadata-ns (make-metadata-namespace)] 
                   #:in-place? [in-place? #f]
                   #:strip [strip-mode #f]
                   #:use-cache? [use-cache? #f]
                   #:quiet? [quiet? #t])
  (define i (stage-package/info (pkg-desc-source desc)
                                (pkg-desc-type desc)
                                (pkg-desc-name desc)
                                #:given-checksum (pkg-desc-checksum desc)
                                #:use-cache? use-cache?
                                #t
                                (if quiet? void printf)
                                metadata-ns
                                #:in-place? in-place?
                                #:strip strip-mode))
  (values (install-info-name i)
          (install-info-directory i)
          (install-info-checksum i)
          (install-info-clean? i)
          (install-info-module-paths i)))

(define (ask question)
  (let loop ()
    (printf question)
    (printf " [Y/n/a/?] ")
    (flush-output)
    (match (string-trim (read-line (current-input-port) 'any))
      [(or "y" "Y" "")
       'yes]
      [(or "n" "N")
       'no]
      [(or "a" "A")
       'always-yes]
      [x
       (eprintf "Invalid answer: ~a\n" x)
       (eprintf " Answer nothing or `y' or `Y' for \"yes\", `n' or `N' for \"no\", or\n")
       (eprintf " `a' or `A' for \"yes for all\".\n")
       (loop)])))

(define (format-deps update-deps)
  (format-list (for/list ([ud (in-list update-deps)])
                 (cond
                  [(pkg-desc? ud)
                   (pkg-desc-name ud)]
                  [(string? ud)
                   ud]
                  [else
                   (format "~a (have ~a, need ~a)"
                           (car ud)
                           (caddr ud)
                           (cadddr ud))]))))

(define (install-packages
         #:old-infos old-infos
         #:old-descs old-descs
         #:pre-succeed pre-succeed
         #:dep-behavior dep-behavior
         #:update-deps? update-deps?
         #:update-implies? update-implies?
         #:update-cache update-cache
         #:updating? updating?
         #:ignore-checksums? ignore-checksums?
         #:use-cache? use-cache?
         #:skip-installed? skip-installed?
         #:force? force?
         #:all-platforms? all-platforms?
         #:quiet? quiet?
         #:from-command-line? from-command-line?
         #:conversation conversation
         #:strip strip-mode
         #:link-dirs? link-dirs?
         #:local-docs-ok? local-docs-ok?
         #:ai-cache ai-cache
         descs)
  (define download-printf (if quiet? void printf/flush))
  (define check-sums? (not ignore-checksums?))
  (define current-scope-db (read-pkg-db))
  (define all-db (merge-pkg-dbs))
  (define path-pkg-cache (make-hash))
  (define (install-package/outer infos desc info)
    (match-define (pkg-desc pkg type orig-name given-checksum auto?) desc)
    (match-define
     (install-info pkg-name orig-pkg pkg-dir clean? checksum module-paths additional-installs)
     info)
    (define name? (eq? 'catalog (first orig-pkg)))
    (define this-dep-behavior (or dep-behavior
                                  (if name?
                                      'search-ask
                                      'fail)))
    (define do-update-deps?
      (and update-deps?
           (member this-dep-behavior '(search-auto search-ask))))
    (define (clean!)
      (when clean?
        (delete-directory/files pkg-dir)))
    (define (show-dependencies deps update? auto?)
      (unless quiet?
        (printf/flush "The following~a packages are listed as dependencies of ~a~a:~a\n"
                      (if update? " out-of-date" " uninstalled")
                      pkg-name
                      (if (or auto? (eq? conversation 'always-yes))
                          (format "\nand they will be ~a~a"
                                  (if auto? "automatically " "")
                                  (if update? "updated" "installed"))
                          "")
                      (if update?
                          (format-deps deps)
                          (format-list deps)))))
    (define simultaneous-installs
      (for/hash ([i (in-list infos)])
        (values (install-info-name i) (install-info-directory i))))
    (cond
      [(and (not updating?)
            (hash-ref all-db pkg-name #f)
            ;; Already installed, but can force if the install is for
            ;; a wider scope:
            (not (and (not (hash-ref current-scope-db pkg-name #f))
                      force?)))
       (define existing-pkg-info (hash-ref all-db pkg-name #f))
       (cond
        [(and (pkg-info-auto? existing-pkg-info)
              (not (pkg-desc-auto? desc))
              ;; Don't confuse a promotion request with a different-source install:
              (equal? (pkg-info-orig-pkg existing-pkg-info) orig-pkg)
              ;; Also, make sure it's installed in the scope that we're changing:
              (hash-ref current-scope-db pkg-name #f))
         ;; promote an auto-installed package to a normally installed one
         (lambda ()
           (unless quiet?
             (download-printf "Promoting ~a from auto-installed to explicitly installed\n" pkg-name))
           (update-pkg-db! pkg-name (update-auto existing-pkg-info #f)))]
        [else
         ;; Fail --- already installed
         (clean!)
         (cond
          [(not (hash-ref current-scope-db pkg-name #f))
           (pkg-error (~a "package is currently installed in a wider scope\n"
                          "  package: ~a\n"
                          "  installed scope: ~a\n"
                          "  given scope: ~a")
                      pkg-name
                      (find-pkg-installation-scope pkg-name #:next? #t)
                      (current-pkg-scope))]
          [(not (equal? (pkg-info-orig-pkg existing-pkg-info) orig-pkg))
           (pkg-error (~a "package is already installed from a different source\n"
                          "  package: ~a\n"
                          "  installed source: ~a\n"
                          "  given source: ~a")
                      pkg-name
                      (pkg-info-orig-pkg existing-pkg-info)
                      orig-pkg)]
          [else
           (pkg-error "package is already installed\n  package: ~a"
                      pkg-name)])])]
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
                     (file-exists? (get-compilation-bytecode-file f))
                     (file-exists? (get-compilation-bytecode-file (path-replace-suffix f #".ss"))))
                 (or (not updating?)
                     (not (equal? pkg-name (path->pkg f #:cache path-pkg-cache)))))
            ;; This module is already installed
            (cons (path->pkg f #:cache path-pkg-cache) mp)]
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
             (pkg-error (~a "packages ~aconflict\n"
                            "  package: ~a\n"
                            "  package: ~a\n"
                            "  module path: ~s")
                        (if (equal? conflicting-pkg pkg-name)
                            "in different scopes "
                            "")
                        pkg conflicting-pkg (pretty-module-path mp))
             (pkg-error (~a "package conflicts with existing installed module\n"
                            "  package: ~a\n"
                            "  module path: ~s")
                        pkg (pretty-module-path mp))))]
      [(and
        (not force?)
        (for/or ([ai (in-set additional-installs)])
          ;; Check for source or compiled:
          (cond
           ;; If `local-docs-ok?`, exempt doc collisions for user-scope install, since
           ;; user-scope documentation is rendered within the package:
           [(and local-docs-ok?
                 (eq? (car ai) 'doc)
                 (eq? (current-pkg-scope) 'user))
            #f]
           [(set-member? (get-additional-installed (car ai)
                                                   simultaneous-installs
                                                   ai-cache
                                                   metadata-ns
                                                   path-pkg-cache)
                         ai)
            ;; This item is already installed
            (cons #f ai)]
           [else
            ;; Compare with simultaneous installs
            (for/or ([other-pkg-info (in-list infos)]
                     #:unless (eq? other-pkg-info info))
              (and (set-member? (install-info-additional-installs other-pkg-info) ai)
                   (cons (install-info-name other-pkg-info)
                         ai)))])))
       =>
       (λ (conflicting-pkg*ai)
         (clean!)
         (match-define (cons conflicting-pkg ai) conflicting-pkg*ai)
         (if conflicting-pkg
             (pkg-error (~a "packages ~aconflict\n"
                            "  package: ~a\n"
                            "  package: ~a\n"
                            "  item category: ~a\n"
                            "  item name: ~s")
                        (if (equal? conflicting-pkg pkg-name)
                            "in different scopes "
                            "")
                        pkg conflicting-pkg
                        (car ai)
                        (cdr ai))
             (pkg-error (~a "package conflicts with existing installed item\n"
                            "  package: ~a\n"
                            "  item category: ~a\n"
                            "  item name: ~s")
                        pkg
                        (car ai)
                        (cdr ai))))]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define deps (get-all-deps metadata-ns pkg-dir))
          (define unsatisfied-deps
            (map dependency->source
                 (filter-not (λ (dep)
                                (define name (dependency->name dep))
                                (or (equal? name "racket")
                                    (not (or all-platforms?
                                             (dependency-this-platform? dep)))
                                    (hash-ref simultaneous-installs name #f)
                                    (hash-has-key? all-db name)))
                             deps)))
          (and (not (empty? unsatisfied-deps))
               unsatisfied-deps)))
       =>
       (λ (unsatisfied-deps)
          (match this-dep-behavior
           ['fail
            (clean!)
            (pkg-error (~a "missing dependencies"
                           (if from-command-line?
                               (~a ";\n"
                                   " specify `--deps search-auto' to install them, or\n"
                                   " specify `--deps search-ask' to be asked about installing them")
                               "")
                           "\n"
                           "  for package: ~a\n"
                           "  missing packages:~a")
                       pkg
                       (format-list unsatisfied-deps))]
           ['search-auto
            ;; (show-dependencies unsatisfied-deps #f #t)
            (raise (vector updating? infos pkg-name unsatisfied-deps void 'always-yes))]
           ['search-ask
            (show-dependencies unsatisfied-deps #f #f)
            (case (if (eq? conversation 'always-yes)
                      'always-yes
                      (ask "Would you like to install these dependencies?"))
              [(yes)
               (raise (vector updating? infos pkg-name unsatisfied-deps void 'again))]
              [(always-yes)
               (raise (vector updating? infos pkg-name unsatisfied-deps void 'always-yes))]
              [(no)
               (clean!)
               (pkg-error "missing dependencies\n  missing packages:~a" (format-list unsatisfied-deps))])]))]
      [(and
        (or do-update-deps?
            update-implies?)
        (let ()
          (define deps (get-all-deps metadata-ns pkg-dir))
          (define implies (list->set
                           (get-all-implies metadata-ns pkg-dir deps)))
          (define update-pkgs
            (append-map (λ (dep)
                           (define name (dependency->name dep))
                           (define this-platform? (or all-platforms?
                                                      (dependency-this-platform? dep)))
                           (or (and this-platform?
                                    (or do-update-deps?
                                        (set-member? implies name))
                                    (not (hash-ref simultaneous-installs name #f))
                                    ((packages-to-update download-printf current-scope-db 
                                                         #:must-update? #f
                                                         #:deps? do-update-deps?
                                                         #:implies? update-implies?
                                                         #:update-cache update-cache
                                                         #:namespace metadata-ns
                                                         #:all-platforms? all-platforms?
                                                         #:ignore-checksums? ignore-checksums?
                                                         #:use-cache? use-cache?
                                                         #:from-command-line? from-command-line?)
                                     name))
                               null))
                        deps))
          (and (not (empty? update-pkgs))
               update-pkgs
               (let ()
                 (define (continue conversation)
                   (raise (vector #t infos pkg-name update-pkgs
                                  (λ () (for-each (compose (remove-package quiet?) pkg-desc-name) update-pkgs))
                                  conversation)))
                 (match (if (andmap (lambda (dep) (set-member? implies (pkg-desc-name dep)))
                                    update-pkgs)
                            'search-auto
                            this-dep-behavior)
                   ['search-auto
                    (show-dependencies update-pkgs #t #t)
                    (continue conversation)]
                   ['search-ask
                    (show-dependencies update-pkgs #t #f)
                    (case (if (eq? conversation 'always-yes)
                              'always-yes
                              (ask "Would you like to update these dependencies?"))
                      [(yes)
                       (continue 'again)]
                      [(always-yes)
                       (continue 'always-yes)]
                      [(no)
                       ;; Don't fail --- just skip update
                       #f])])))))
       (error "internal error: should have raised an exception")]
      [(and
        (not (eq? dep-behavior 'force))
        (let ()
          (define deps (get-all-deps metadata-ns pkg-dir))
          (define update-deps
            (filter-map (λ (dep)
                          (define name (dependency->name dep))
                          (define req-vers (dependency->version dep))
                          (define this-platform? (or all-platforms?
                                                     (dependency-this-platform? dep)))
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
                              (values (get-metadata metadata-ns (pkg-directory** name)
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
            ;; If there's a mismatch that we can't attempt to update, complain.
            (unless (andmap cadr update-deps)
              (report-mismatch (filter (compose not cadr) update-deps)))
            ;; Try updates:
            (define update-pkgs (map car update-deps))
            (define (make-pre-succeed)
              (define db current-scope-db)
              (let ([to-update (append-map (packages-to-update download-printf db
                                                               #:deps? update-deps? 
                                                               #:implies? update-implies?
                                                               #:update-cache update-cache
                                                               #:namespace metadata-ns
                                                               #:all-platforms? all-platforms?
                                                               #:ignore-checksums? ignore-checksums?
                                                               #:use-cache? use-cache?
                                                               #:from-command-line? from-command-line?)
                                           update-pkgs)])
                (λ () (for-each (compose (remove-package quiet?) pkg-desc-name) to-update))))
            (match this-dep-behavior
              ['fail
               (clean!)
               (report-mismatch update-deps)]
              ['search-auto
               (show-dependencies update-deps #t #t)
               (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'always-yes))]
              ['search-ask
               (show-dependencies update-deps #t #f)
               (case (if (eq? conversation 'always-yes)
                         'always-yes
                         (ask "Would you like to update these dependencies?"))
                 [(yes)
                  (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'again))]
                 [(always-yes)
                  (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'always-yes))]
                 [(no)
                  (clean!)
                  (report-mismatch update-deps)])]))]
      [else
       (λ ()
         (when updating?
           (download-printf "Re-installing ~a\n" pkg-name))
         (define final-pkg-dir
           (cond
             [clean?
              (define final-pkg-dir (select-package-directory
                                     (build-path (pkg-installed-dir) pkg-name)))
              (make-parent-directory* final-pkg-dir)
              (copy-directory/files pkg-dir final-pkg-dir #:keep-modify-seconds? #t)
              (clean!)
              final-pkg-dir]
             [else
              pkg-dir]))
         (define single-collect (pkg-single-collection final-pkg-dir
                                                       #:name pkg-name
                                                       #:namespace post-metadata-ns))
         (log-pkg-debug "creating ~alink to ~e" 
                        (if single-collect "single-collection " "") 
                        final-pkg-dir)
         (define scope (current-pkg-scope))
         (links final-pkg-dir
                #:name single-collect
                #:user? (not (or (eq? 'installation scope)
                                 (path? scope)))
                #:file (scope->links-file scope)
                #:root? (not single-collect)
                #:static-root? (and (pair? orig-pkg)
                                    (eq? 'static-link (car orig-pkg))))
         (define alt-dir-name
           ;; If we had to pick an alternate dir name, then record it:
           (let-values ([(base name dir?) (split-path final-pkg-dir)])
             (and (regexp-match? #rx"[+]" name)
                  (path->string name))))
         (define this-pkg-info
           (make-pkg-info orig-pkg checksum auto? single-collect alt-dir-name))
         (log-pkg-debug "updating db with ~e to ~e" pkg-name this-pkg-info)
         (update-pkg-db! pkg-name this-pkg-info))]))
  (define metadata-ns (make-metadata-namespace))
  (define infos
    (for/list ([v (in-list descs)])
      (stage-package/info (pkg-desc-source v) (pkg-desc-type v) (pkg-desc-name v)
                          #:given-checksum (pkg-desc-checksum v)
                          #:use-cache? use-cache?
                          check-sums? download-printf
                          metadata-ns
                          #:strip strip-mode
                          #:link-dirs? link-dirs?)))
  ;; For the top-level call, we need to double-check that all provided packages
  ;; were distinct:
  (for/fold ([ht (hash)]) ([i (in-list infos)]
                           [desc (in-list descs)])
    (define name (install-info-name i))
    (when (hash-ref ht name #f)
      (pkg-error (~a "given package sources have the same package name\n"
                     "  package name: ~a\n"
                     "  package source: ~a\n"
                     "  package source: ~a")
                 name
                 (pkg-desc-source (hash-ref ht name #f))
                 (pkg-desc-source desc)))
    (hash-set ht name desc))

  (define all-descs (append old-descs descs))
  (define all-infos (append old-infos infos))

  (define do-its
    (map (curry install-package/outer all-infos)
         all-descs
         all-infos))
  (pre-succeed)

  (define post-metadata-ns (make-metadata-namespace))
  (for-each (λ (t) (t)) do-its)

  (define (is-promote? info)
    ;; if the package name is in `current-scope-db', we must
    ;; be simply promiting the package, and so it's
    ;; already set up:
    (and (hash-ref current-scope-db (install-info-name info) #f) #t))

  (define setup-collects
    (let ([db (read-pkg-db)])
      (get-setup-collects ((if updating?
                               (make-close-over-depending (read-pkg-db)
                                                          post-metadata-ns
                                                          all-platforms?)
                               values)
                           (map install-info-name
                                (if updating?
                                    all-infos
                                    (filter-not is-promote? all-infos))))
                          db
                          post-metadata-ns)))

  (cond
   [(or (null? do-its)
        (and (not updating?) (andmap is-promote? all-infos)))
    ;; No actions, so no setup:
    'skip]
   [else
    setup-collects]))

(define (pkg-single-collection dir 
                               #:name [pkg-name (let-values ([(base name dir?) (split-path dir)])
                                                  (path-element->string name))]
                               #:namespace [metadata-ns (make-metadata-namespace)])
  (define i (get-pkg-info dir metadata-ns))
  (if (not i)
      pkg-name
      (let ([s (i 'collection (lambda () 'use-pkg-name))])
        (unless (or (collection-name-element? s)
                    (eq? s 'multi)
                    (eq? s 'use-pkg-name))
          (log-error (format (~a "bad `collection' definition in \"info.rkt\";\n"
                                 " definition will be ignored\n"
                                 "  path: ~a\n"
                                 "  found: ~e\n"
                                 "  expected: (or/c collection-name-element? 'multi 'use-pkg-name)")
                             (build-path dir "info.rkt")
                             s)))
        (or (and (collection-name-element? s)
                 s)
            (and (eq? s 'use-pkg-name)
                 pkg-name)))))

(define (get-setup-collects pkg-names db metadata-ns)
  (maybe-append
   (for/list ([pkg-name (in-list pkg-names)])
     (define pkg-dir (pkg-directory* pkg-name #:db db))
     (define single-collect
       (and pkg-dir
            (pkg-single-collection pkg-dir #:name pkg-name #:namespace metadata-ns)))
     (or (and (not pkg-dir) null)
         (and single-collect (list single-collect))
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

(define ((make-close-over-depending db metadata-ns all-platforms?) l)
  (define setup-pkgs (list->set l))
  (define empty-set (set))
  (define rev-pkg-deps
    (for/fold ([rev (hash)]) ([pkg-name (in-hash-keys db)])
      (for/fold ([rev rev]) ([dep (in-list ((package-dependencies metadata-ns db all-platforms?)
                                            pkg-name))])
        (hash-update rev dep (lambda (v) (set-add v pkg-name)) empty-set))))
  (let loop ([check setup-pkgs] [setup-pkgs setup-pkgs])
    ;; Find all packages that depend on a package in `check':
    (define new-check 
      (set-subtract (for/fold ([new-check (set)]) ([pkg (in-set check)])
                      (set-union new-check 
                                 (hash-ref rev-pkg-deps pkg empty-set)))
                    setup-pkgs))
    (cond
     [(set-empty? new-check)
      ;; found fixed point:
      (set->list setup-pkgs)]
     [else
      ;; more packages to setup and check:
      (loop new-check
            (set-union setup-pkgs new-check))])))

(define (select-package-directory dir #:counter [counter 0])
  (define full-dir (if (zero? counter)
                       dir
                       (let-values ([(base name dir?) (split-path dir)])
                         (define new-name (bytes->path
                                           (bytes-append (path->bytes name)
                                                         (string->bytes/utf-8
                                                          (~a "+" counter)))))
                         (if (path? base)
                             (build-path base new-name)
                             new-name))))
  (cond
   [(directory-exists? full-dir)
    ;; If the directory exists, assume that we'd like to replace it.
    ;; Maybe the directory couldn't be deleted when a package was
    ;; uninstalled, and maybe it will work now (because some process
    ;; has completed on Windows or some other filesystem with locks).
    (with-handlers ([exn:fail:filesystem?
                     (lambda (exn)
                       (log-pkg-warning "error deleting old directory: ~a" 
                                        (exn-message exn))
                       (select-package-directory dir #:counter (add1 counter)))])
      (delete-directory/files full-dir)
      ;; delete succeeded:
      full-dir)]
   [else
    ;; all clear to use the selected name:
    full-dir]))

(define (snoc l x)
  (append l (list x)))

(define (pkg-install descs
                     #:old-infos [old-infos empty]
                     #:old-auto+pkgs [old-descs empty]
                     #:all-platforms? [all-platforms? #f]
                     #:force? [force #f]
                     #:ignore-checksums? [ignore-checksums? #f]
                     #:strict-doc-conflicts? [strict-doc-conflicts? #f]
                     #:use-cache? [use-cache? #t]
                     #:skip-installed? [skip-installed? #f]
                     #:pre-succeed [pre-succeed void]
                     #:dep-behavior [dep-behavior #f]
                     #:update-deps? [update-deps? #f]
                     #:update-implies? [update-implies? #t]
                     #:update-cache [update-cache (make-hash)]
                     #:updating? [updating? #f]
                     #:quiet? [quiet? #f]
                     #:from-command-line? [from-command-line? #f]
                     #:conversation [conversation #f]
                     #:strip [strip-mode #f]
                     #:link-dirs? [link-dirs? #f]
                     #:summary-deps [summary-deps empty])
  (define new-descs
    (remove-duplicates
     (if (not skip-installed?)
         descs
         (let ([db (read-pkg-db)])
           (filter (lambda (d)
                     (define pkg-name
                       (or (pkg-desc-name d)
                           (package-source->name (pkg-desc-source d) 
                                                 (pkg-desc-type d))))
                     (define i (hash-ref db pkg-name #f))
                     (or (not i) (pkg-info-auto? i)))
                   descs)))
     pkg-desc=?))
  (with-handlers* ([vector?
                    (match-lambda
                     [(vector updating? new-infos dep-pkg deps more-pre-succeed conv)
                      (pkg-install
                       #:summary-deps (snoc summary-deps  (vector dep-pkg deps))
                       #:old-infos new-infos
                       #:old-auto+pkgs (append old-descs new-descs)
                       #:all-platforms? all-platforms?
                       #:force? force
                       #:ignore-checksums? ignore-checksums?
                       #:strict-doc-conflicts? strict-doc-conflicts?
                       #:use-cache? use-cache?
                       #:dep-behavior dep-behavior
                       #:update-deps? update-deps?
                       #:update-implies? update-implies?
                       #:update-cache update-cache
                       #:pre-succeed (lambda () (pre-succeed) (more-pre-succeed))
                       #:updating? updating?
                       #:conversation conv
                       #:strip strip-mode
                       (for/list ([dep (in-list deps)])
                         (if (pkg-desc? dep)
                             dep
                             (pkg-desc dep #f #f #f #t))))])])
    (begin0
      (install-packages
       #:old-infos old-infos
       #:old-descs old-descs
       #:all-platforms? all-platforms?
       #:force? force
       #:ignore-checksums? ignore-checksums?
       #:use-cache? use-cache?
       #:skip-installed? skip-installed?
       #:dep-behavior dep-behavior
       #:update-deps? update-deps?
       #:update-implies? update-implies?
       #:update-cache update-cache
       #:pre-succeed pre-succeed
       #:updating? updating?
       #:quiet? quiet?
       #:from-command-line? from-command-line?
       #:conversation conversation
       #:strip strip-mode
       #:link-dirs? link-dirs?
       #:local-docs-ok? (not strict-doc-conflicts?)
       #:ai-cache (box #f)
       new-descs)
      (unless (empty? summary-deps)
        (unless quiet?
          (printf/flush "The following~a packages were listed as dependencies~a:~a\n"
                        (if updating? " out-of-date" " uninstalled")
                        (format "\nand they were ~a~a"
                                (if (eq? dep-behavior 'search-auto) "automatically " "")
                                (if updating? "updated" "installed"))
                        (string-append*
                         (for/list ([p*ds (in-list summary-deps)])
                           (match-define (vector n ds) p*ds)
                           (format "\n dependencies of ~a:~a"
                                   n
                                   (if updating?
                                     (format-deps ds)
                                     (format-list ds)))))))))))

;; Determine packages to update, starting with `pkg-name'. If `pkg-name'
;; needs to be updated, return it in a list. Otherwise, if `deps?',
;; then return a list of dependencies that need to be updated.
;; (If a package needs to be updated, wait until the update
;; has been inspected for further dependencies.)
;; If `must-installed?', then complain if the package is not
;; installed inthe current scope.
;; If `must-update?', then complain if the package is not
;; updatable.
;; The `update-cache' argument is used to cache which packages
;; are already being updated and downloaded checksums.
(define ((packages-to-update download-printf db
                             #:must-installed? [must-installed? #t]
                             #:must-update? [must-update? #t]
                             #:deps? deps?
                             #:implies? implies?
                             #:namespace metadata-ns 
                             #:update-cache update-cache
                             #:all-platforms? all-platforms?
                             #:ignore-checksums? ignore-checksums?
                             #:use-cache? use-cache?
                             #:from-command-line? from-command-line?)
         pkg-name)
  (cond
   [(pkg-desc? pkg-name)
    ;; Infer the package-source type and name:
    (define-values (inferred-name type) (package-source->name+type
                                         (pkg-desc-source pkg-name)
                                         (pkg-desc-type pkg-name)
                                         #:must-infer-name? (not (pkg-desc-name pkg-name))
                                         #:complain complain-about-source))
    (define name (or (pkg-desc-name pkg-name)
                     inferred-name))
    ;; Check that the package is installed, and get current checksum:
    (define info (package-info name #:db db))
    (define new-checksum (checksum-for-pkg-source (pkg-desc-source pkg-name)
                                                  type
                                                  name
                                                  (pkg-desc-checksum pkg-name)
                                                  download-printf))
    (unless (or ignore-checksums? (not (pkg-desc-checksum pkg-name)))
      (unless (equal? (pkg-desc-checksum pkg-name) new-checksum)
        (pkg-error (~a "incorrect checksum on package\n"
                       "  package source: ~a\n"
                       "  expected: ~e\n"
                       "  got: ~e")
                   (pkg-desc-source pkg-name)
                   (pkg-desc-checksum pkg-name) 
                   new-checksum)))
    (if (or (not (equal? (pkg-info-checksum info)
                         new-checksum))
            ;; No checksum available => always update
            (not new-checksum))
        ;; Update:
        (begin
          (hash-set! update-cache (pkg-desc-source pkg-name) #t)
          (list (pkg-desc (pkg-desc-source pkg-name)
                          (pkg-desc-type pkg-name)
                          name
                          (pkg-desc-checksum pkg-name)
                          (pkg-desc-auto? pkg-name))))
        ;; No update needed, but maybe check dependencies:
        (if (or deps?
                implies?)
            ((packages-to-update download-printf db
                                 #:must-update? #f
                                 #:deps? deps?
                                 #:implies? implies?
                                 #:update-cache update-cache
                                 #:namespace metadata-ns
                                 #:all-platforms? all-platforms?
                                 #:ignore-checksums? ignore-checksums?
                                 #:use-cache? use-cache?
                                 #:from-command-line? from-command-line?)
             name)
            null))]
   [(eq? #t (hash-ref update-cache pkg-name #f))
    ;; package is already being updated
    null]
   ;; A string indicates that package source that should be
   ;; looked up in the installed packages to get the old source
   ;; for getting the checksum:
   [(package-info pkg-name #:db db must-update?)
    =>
    (lambda (m)
      (match-define (pkg-info orig-pkg checksum auto?) m)
      (match orig-pkg
        [`(,(or 'link 'static-link) ,orig-pkg-dir)
         (if must-update?
             (pkg-error (~a "cannot update linked packages~a\n"
                            "  package name: ~a\n"
                            "  package source: ~a")
                        (if from-command-line?
                            " without `--link'"
                            " without new link")
                        pkg-name
                        (normalize-path
                         (path->complete-path orig-pkg-dir (pkg-installed-dir))))
             null)]
        [`(dir ,_)
         (if must-update?
             (pkg-error (~a "cannot update packages installed locally;\n"
                            " package was installed via a local directory\n"
                        "  package name: ~a")
                        pkg-name)
             null)]
        [`(file ,_)
         (if must-update?
             (pkg-error (~a "cannot update packages installed locally;\n"
                            " package was installed via a local file\n"
                            "  package name: ~a")
                        pkg-name)
             null)]
        [`(,_ ,orig-pkg-source)
         (define new-checksum
           (or (hash-ref update-cache pkg-name #f)
               (remote-package-checksum orig-pkg download-printf pkg-name)))
         ;; Record downloaded checksum:
         (hash-set! update-cache pkg-name new-checksum)
         (or (and new-checksum
                  (not (equal? checksum new-checksum))
                  (begin
                    ;; Update it:
                    (hash-set! update-cache pkg-name #t)
                    ;; Flush cache of downloaded checksums, in case
                    ;; there was a race between our checkig and updates on
                    ;; the catalog server:
                    (clear-checksums-in-cache! update-cache)
                    ;; FIXME: the type shouldn't be #f here; it should be
                    ;; preseved from install time:
                    (list (pkg-desc orig-pkg-source #f pkg-name #f auto?))))
             (if (or deps? implies?)
                 ;; Check dependencies
                 (append-map
                  (packages-to-update download-printf db
                                      #:must-update? #f
                                      #:deps? deps?
                                      #:implies? implies?
                                      #:update-cache update-cache
                                      #:namespace metadata-ns
                                      #:all-platforms? all-platforms?
                                      #:ignore-checksums? ignore-checksums?
                                      #:use-cache? use-cache?
                                      #:from-command-line? from-command-line?)
                  ((package-dependencies metadata-ns db all-platforms? 
                                         #:only-implies? (not deps?))
                   pkg-name))
                 null))]))]
   [else null]))

(define (clear-checksums-in-cache! update-cache)
  (define l (for/list ([(k v) (in-hash update-cache)]
                       #:when (string? v))
              k))
  (for ([k (in-list l)]) (hash-remove! update-cache k)))
                  

(define ((package-dependencies metadata-ns db all-platforms?
                               #:only-implies? [only-implies? #f]) 
         pkg-name)
  (define pkg-dir (pkg-directory* pkg-name #:db db))
  (define deps
    (map dependency->name 
         (let ([l (get-all-deps metadata-ns pkg-dir)])
           (if all-platforms?
               l
               (filter dependency-this-platform? l)))))
  (if only-implies?
      (let ([implies (list->set (get-all-implies metadata-ns pkg-dir deps))])
        (filter (lambda (dep)
                  (set-member? implies dep))
                deps))
      deps))

(define (pkg-update in-pkgs
                    #:all? [all? #f]
                    #:dep-behavior [dep-behavior #f]
                    #:all-platforms? [all-platforms? #f]
                    #:force? [force? #f]
                    #:ignore-checksums? [ignore-checksums? #f]
                    #:strict-doc-conflicts? [strict-doc-conflicts? #f]
                    #:use-cache? [use-cache? #t]
                    #:update-deps? [update-deps? #f]
                    #:update-implies? [update-implies? #t]
                    #:quiet? [quiet? #f]
                    #:from-command-line? [from-command-line? #f]
                    #:strip [strip-mode #f]
                    #:link-dirs? [link-dirs? #f])
  (define download-printf (if quiet? void printf))
  (define metadata-ns (make-metadata-namespace))
  (define db (read-pkg-db))
  (define all-mode? (and all? (empty? in-pkgs)))
  (define pkgs (cond
                [all-mode? (hash-keys db)]
                [else in-pkgs]))
  (define update-cache (make-hash))
  (define to-update (append-map (packages-to-update download-printf db
                                                    #:must-update? (not all-mode?)
                                                    #:deps? (or update-deps? 
                                                                all-mode?) ; avoid races
                                                    #:implies? update-implies?
                                                    #:update-cache update-cache
                                                    #:namespace metadata-ns
                                                    #:all-platforms? all-platforms?
                                                    #:ignore-checksums? ignore-checksums?
                                                    #:use-cache? use-cache?
                                                    #:from-command-line? from-command-line?)
                                pkgs))
  (cond
    [(empty? pkgs)
     (unless quiet?
       (printf/flush (~a "No packages given to update"
                         (if from-command-line?
                             ";\n use `--all' to update all packages"
                             "")
                         "\n")))
     'skip]
    [(empty? to-update)
     (unless quiet?
       (printf/flush "No updates available\n"))
     'skip]
    [else
     (unless quiet?
       (printf "Updating:\n")
       (for ([u (in-list to-update)])
         (printf "  ~a\n" (pkg-desc-name u)))
       (flush-output))
     (pkg-install
      #:updating? #t
      #:pre-succeed (λ () (for-each (compose (remove-package quiet?) pkg-desc-name) to-update))
      #:dep-behavior dep-behavior
      #:update-deps? update-deps?
      #:update-implies? update-implies?
      #:update-cache update-cache
      #:quiet? quiet?
      #:from-command-line? from-command-line?
      #:strip strip-mode
      #:all-platforms? all-platforms?
      #:force? force?
      #:ignore-checksums? ignore-checksums?
      #:strict-doc-conflicts? strict-doc-conflicts?
      #:use-cache? use-cache?
      #:link-dirs? link-dirs?
      to-update)]))

(define (pkg-show indent 
                  #:directory? [dir? #f]
                  #:auto? [show-auto? #f])
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (if (null? pkgs)
        (printf " [none]\n")
        (begin
          (table-display
           (list*
            (append
             (list (format "~aPackage~a"
                           indent 
                           (if show-auto? "[*=auto]" ""))
                   "Checksum"
                   "Source")
             (if dir?
                 (list "Directory")
                 empty))
            (for/list ([pkg (in-list pkgs)]
                       #:when (or show-auto?
                                  (not (pkg-info-auto? (hash-ref db pkg)))))
              (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
              (append
               (list (format "~a~a~a"
                             indent
                             pkg
                             (if auto? "*" ""))
                     (format "~a" checksum)
                     (format "~a" orig-pkg))
               (if dir?
                   (list (~a (pkg-directory* pkg #:db db)))
                   empty)))))
          (unless show-auto?
            (define n (for/sum ([pkg (in-list pkgs)] 
                                #:when (pkg-info-auto? (hash-ref db pkg)))
                               1))
            (unless (zero? n)
              (printf "~a[~a auto-installed package~a not shown]\n"
                      indent
                      n
                      (if (= n 1) "" "s"))))))))

(define (installed-pkg-table #:scope [given-scope #f])
  (parameterize ([current-pkg-scope 
                  (or given-scope (default-pkg-scope))])
    (with-pkg-lock/read-only
     (read-pkg-db))))

(define (installed-pkg-names #:scope [given-scope #f])
  (sort (hash-keys (installed-pkg-table #:scope given-scope))
        string-ci<=?))
  
(define (pkg-migrate from-version
                     #:all-platforms? [all-platforms? #f]
                     #:force? [force? #f]
                     #:quiet? [quiet? #f]
                     #:from-command-line? [from-command-line? #f]
                     #:ignore-checksums? [ignore-checksums? #f]
                     #:strict-doc-conflicts? [strict-doc-conflicts? #f]
                     #:use-cache? [use-cache? #t]
                     #:dep-behavior [dep-behavior #f]
                     #:strip [strip-mode #f])
  (define from-db
    (parameterize ([current-pkg-scope-version from-version])
      (installed-pkg-table #:scope 'user)))
  (define to-install
    (sort
     (for/list ([(name info) (in-hash from-db)]
                #:unless (pkg-info-auto? info))
       (define-values (source type)
         (match (pkg-info-orig-pkg info)
           [(list 'catalog name) (values name 'name)]
           [(list 'url url) (values url #f)]
           [(list 'link path) (values path 'link)]
           [(list 'static-link path) (values path 'static-link)]))
       (pkg-desc source type name #f #f))
     string<?
     #:key pkg-desc-name))
  (unless quiet?
    (cond
     [(null? to-install)
      (printf "No packages from ~s to install\n" from-version)]
     [else
      (printf "Packages to install:\n")
      (for ([d (in-list to-install)])
        (define t (pkg-desc-type d))
        (define n (pkg-desc-name d))
        (case t
          [(name) (printf "  ~a\n" n)]
          [(link static-link)
           (printf "  ~a ~aed from ~a\n" n t (pkg-desc-source d))]
          [else
           (printf "  ~a from ~a\n" n (pkg-desc-source d))]))]))
  (if (null? to-install)
      'skip
      (begin0
       (pkg-install to-install
                    #:all-platforms? all-platforms?
                    #:force? force?
                    #:ignore-checksums? ignore-checksums?
                    #:strict-doc-conflicts? strict-doc-conflicts?
                    #:use-cache? use-cache?
                    #:skip-installed? #t
                    #:dep-behavior (or dep-behavior 'search-auto)
                    #:quiet? quiet? 
                    #:from-command-line? from-command-line?
                    #:strip strip-mode)
       (unless quiet?
         (printf "Packages migrated\n")))))

(define (pkg-config config:set key+vals
                    #:from-command-line? [from-command-line? #f])
  (cond
    [config:set
     (match key+vals
       [(list)
        (pkg-error "no config key given")]
       [(list (and key
                   (or "default-scope"
                       "name"
                       "download-cache-max-files"
                       "download-cache-max-bytes"
                       "download-cache-dir"
                       "doc-open-url")))
        (pkg-error (~a "missing value for config key\n"
                       "  config key: ~a")
                   key)]
       [(list* (and key
                    (or "default-scope"
                        "name"
                        "download-cache-max-files"
                        "download-cache-max-bytes"
                        "download-cache-dir"))
               val
               another-val
               more-vals)
        (pkg-error (~a "too many values provided for config key\n"
                       "  config key: ~a\n"
                       "  given values:~a")
                   key
                   (format-list (cons val more-vals)))]
       [(list* (and key "catalogs") val)
        (update-pkg-cfg! 'catalogs val)]
       [(list (and key "default-scope") val)
        (unless (member val '("installation" "user"))
          (pkg-error (~a "invalid value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: installation, user")
                     key
                     val))
        (update-pkg-cfg! 'default-scope val)]
       [(list (and key "name") val)
        (unless (eq? 'installation (current-pkg-scope))
          (pkg-error (~a "setting `name' makes sense only in `installation' scope\n"
                         "  current package scope: ~a")
                     (current-pkg-scope)))
        (update-pkg-cfg! 'installation-name val)]
       [(list (and key "download-cache-dir")
              val)
        (unless (complete-path? val)
          (pkg-error (~a "invalid value for config key\n"
                         " not an absolute path\n"
                         "  config key: ~a\n"
                         "  given value: ~a")
                     key
                     val))
        (update-pkg-cfg! (string->symbol key) val)]
       [(list (and key (or "download-cache-max-files"
                           "download-cache-max-bytes"))
              val)
        (unless (real? (string->number val))
          (pkg-error (~a "invalid value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: real numbers")
                     key
                     val))
        (update-pkg-cfg! (string->symbol key) (string->number val))]
       [(list (and key "doc-open-url") val)
        (unless (eq? 'installation (current-pkg-scope))
          (pkg-error (~a "setting `doc-open-url' works only in `installation' scope\n"
                         "  current package scope: ~a")
                     (current-pkg-scope)))
        (update-pkg-cfg! 'doc-open-url (if (equal? val "") #f val))]
       [(list* key args)
        (pkg-error "unsupported config key\n  key: ~a" key)])]
    [else
     (define (show key+vals indent)
       (match key+vals
         [(list key)
          (match key
            ["catalogs"
             (for ([s (in-list (read-pkg-cfg/def 'catalogs))])
               (printf "~a~a\n" indent s))]
            ["default-scope"
             (printf "~a~a\n" indent (read-pkg-cfg/def 'default-scope))]
            ["name"
             (printf "~a~a\n" indent (read-pkg-cfg/def 'installation-name))]
            [(or "download-cache-dir"
                 "download-cache-max-files"
                 "download-cache-max-bytes")
             (printf "~a~a\n" indent (read-pkg-cfg/def (string->symbol key)))]
            ["doc-open-url"
             (printf "~a~a\n" indent (or (read-pkg-cfg/def 'doc-open-url) ""))]
            [_
             (pkg-error "unsupported config key\n  key: ~e" key)])]
         [(list)
          (pkg-error "config key not provided")]
         [_
          (pkg-error (~a "multiple config keys provided"
                         (if from-command-line?
                             ";\n supply `--set' to set a config key's value"
                             "")))]))
     (match key+vals
       [(list)
        (for ([key (in-list '("name"
                              "catalogs"
                              "default-scope"
                              "download-cache-dir"
                              "download-cache-max-files"
                              "download-cache-max-bytes"))])
          (printf "~a:\n" key)
          (show (list key) "  "))]
       [_ (show key+vals "")])]))

(define (create-as-is create:format pkg-name dir orig-dir
                      #:quiet? [quiet? #f]
                      #:from-command-line? [from-command-line? #f]
                      #:hide-src? [hide-src? #f]
                      #:dest [dest-dir #f])
  (begin
    (unless (directory-exists? dir)
      (pkg-error "directory does not exist\n  path: ~a" dir))
    (match create:format
      ['MANIFEST
       (unless quiet?
         (printf/flush "creating manifest for ~a\n"
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
       (define actual-dest-dir (if dest-dir
                                   (path->complete-path dest-dir)
                                   (let-values ([(base name dir?) (split-path dir)])
                                     (cond
                                      [(path? base) (path->complete-path base)]
                                      [else (current-directory)]))))
       (define pkg/complete (path->complete-path pkg actual-dest-dir))
       ;; To make checksums more consistent, set a directory's timestamp to
       ;; the latest time of any of its source files.
       (define (use-real-timestamp? p)
         (and (file-exists? p)
              (regexp-match? #rx"[.](?:rkt|ss|scrbl|txt)$" p)))
       (define latest-timestamp
         (for/fold ([ts #f]) ([f (in-directory dir)])
           (define fts (and (use-real-timestamp? f)
                            (file-or-directory-modify-seconds f)))
           (if (and fts (or (not ts) (fts . > . ts)))
               fts
               ts)))
       (define (file-or-directory-timestamp p)
         (or (and (not (use-real-timestamp? p))
                  latest-timestamp)
             (file-or-directory-modify-seconds p)))
       (unless quiet?
         (printf/flush "packing~a into ~a\n"
                       (if hide-src? "" (format " ~a" dir))
                       (if dest-dir
                           pkg/complete
                           pkg)))
       (define (add-directory-layer? content)
         ;; We need to add a layer for zip/tgz if the package content
         ;; is a single directory, which is an unlikely case.
         ;; That mode is not compatble with Racket v60.0.1.12 and earlier.
         ;; When only Racket v6.0.1.12 is later is relevant,
         ;; we might prefer to always add a layer for consistency and
         ;; because it's nicer for manual unpacking.
         (and (= 1 (length content))
              (directory-exists? (car content))))
       (match create:format
         ['tgz
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (define content (directory-list))
              (apply tar-gzip pkg/complete content
                     #:path-prefix (and (add-directory-layer? content)
                                        pkg-name)
                     #:get-timestamp file-or-directory-timestamp)))]
         ['zip
          (when (file-exists? pkg/complete)
            (delete-file pkg/complete))
          (parameterize ([current-directory dir])
            (with-handlers ([exn? (lambda (exn)
                                    (when (file-exists? pkg/complete)
                                      (delete-file pkg/complete))
                                    (raise exn))])
              (define content (directory-list))
              (apply zip pkg/complete content
                     #:path-prefix (and (add-directory-layer? content)
                                        pkg-name)
                     #:get-timestamp file-or-directory-timestamp
                     #:utc-timestamps? #t
                     #:round-timestamps-down? #t)))]
         ['plt
          (define dest pkg/complete)
          (when (pkg-single-collection #:name pkg-name dir)
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
         (printf/flush "writing package checksum to ~a\n"
                       (if dest-dir
                           chk/complete
                           chk)))
       (with-output-to-file chk/complete
         #:exists 'replace
         (λ () (display (call-with-input-file pkg/complete sha1))))])))

(define (stripped-create mode name dir
                        #:format [create:format 'zip]
                        #:quiet? [quiet? #f]
                        #:from-command-line? [from-command-line? #f]
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
                      #:from-command-line? from-command-line?
                      #:dest (if archive-dest-dir
                                 (path->complete-path archive-dest-dir)
                                 (current-directory))))
      (lambda ()
        (delete-directory/files tmp-dir))))

(define (pkg-create create:format dir-or-name
                    #:pkg-name [given-pkg-name #f]
                    #:dest [dest-dir #f]
                    #:source [source 'dir]
                    #:mode [mode 'as-is]
                    #:quiet? [quiet? #f]
                    #:from-command-line? [from-command-line? #f])
  (define pkg-name
    (or given-pkg-name
        (if (eq? source 'dir)
            (path->string (let-values ([(base name dir?) (split-path dir-or-name)])
                            name))
            dir-or-name)))
  (define dir
    (if (eq? source 'dir)
        dir-or-name
        (let ()
          (define (get-dir scope)
            (parameterize ([current-pkg-scope scope])
              (with-pkg-lock/read-only
               (pkg-directory* dir-or-name))))
          (define dir (get-dir 'user))
          (unless dir
            (pkg-error (~a "package not installed in user scope\n"
                           "  package name: ~a"
                           (if (get-dir 'installation)
                               "\n  installed in scope: installation"
                               ""))
                       dir-or-name))
          dir)))
  (case mode
    [(as-is)
     (create-as-is create:format pkg-name dir dir
                   #:dest dest-dir
                   #:quiet? quiet?
                   #:from-command-line? from-command-line?)]
    [else (stripped-create mode pkg-name dir
                           #:dest dest-dir
                           #:format create:format
                           #:quiet? quiet?
                           #:from-command-line? from-command-line?)]))

(define (src->url-or-path src)
  (cond
   [(path? src) (path->complete-path src)]
   [(regexp-match? #rx"^https?://" src)
    (string->url src)]
   [(regexp-match? #rx"^file://" src)
    (url->path (string->url src))]
   [(regexp-match? #rx"^[a-zA-Z]*://" src)
    (pkg-error (~a "unrecognized URL scheme for a catalog\n"
                   "  URL: ~a")
               src)]
   [else (path->complete-path src)]))

(define (url-or-path->url-string p)
  (url->string (if (url? p)
                   p
                   (path->url p))))

(define (pkg-catalog-copy srcs dest
                          #:from-config? [from-config? #f]
                          #:merge? [merge? #f]
                          #:force? [force? #f]
                          #:override? [override? #f]
                          #:relative-sources? [relative-sources? #f])
  (define src-paths
    (for/list ([src (in-list (append srcs
                                     (if from-config?
                                         (pkg-config-catalogs)
                                         null)))])
      (define src-path (src->url-or-path src))
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

  (define dest-dir
    (and relative-sources?
         (if (db-path? dest-path)
             (let-values ([(base name dir?) (split-path dest-path)])
               base)
             dest-path)))

  (unless (or force? merge?)
    (when (or (file-exists? dest-path)
              (directory-exists? dest-path)
              (link-exists? dest-path))
      (pkg-error (~a "destination exists\n"
                     "  path: ~a")
                 dest-path)))

  (define absolute-details
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
  (define details
    (if relative-sources?
        (for/hash ([(k ht) (in-hash absolute-details)])
          (values k (source->relative-source dest-dir ht)))
        absolute-details))

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
    (define vers-details
      (for/hash ([(k v) (in-hash details)])
        (values k (select-info-version v))))
    (parameterize ([db:current-pkg-catalog-file dest-path])
      (db:set-catalogs! '("local"))
      (db:set-pkgs! "local"
                    (for/list ([(k v) (in-hash vers-details)])
                      (db:pkg k "local"
                              (hash-ref v 'author "")
                              (hash-ref v 'source "")
                              (hash-ref v 'checksum "")
                              (hash-ref v 'description ""))))
      (for ([(k v) (in-hash vers-details)])
        (define t (hash-ref v 'tags '()))
        (unless (null? t)
          (db:set-pkg-tags! k "local" t)))
      (for ([(k v) (in-hash vers-details)])
        (define mods (hash-ref v 'modules '()))
        (unless (null? mods)
          (define cs (hash-ref v 'checksum ""))
          (db:set-pkg-modules! k "local" cs mods)))
      (for ([(k v) (in-hash vers-details)])
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
                             (sort (hash-keys all-details) string<?)
                             names))]
          [position (in-naturals)])
      (define details (select-info-version
                       (if all?
                           (hash-ref all-details name)
                           (get-pkg-details-from-catalogs name))))
      (unless (zero? position) (newline))
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
        (printf " Modules:")
        (for/fold ([col 72]) ([mod (in-list (hash-ref details 'modules null))])
          (define pretty-mod (pretty-module-path mod))
          (define mod-str (~a " " (~s pretty-mod)))
          (define new-col (if ((+ col (string-length mod-str)) . > . 72)
                              (begin
                                (printf "\n ")
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
    (package-catalog-lookup name #t #f)))

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
    (unless one-ht
      (pkg-error (~a "could not read package catalog\n"
                     "  catalog: ~a")
                 (url->string i)))
    (for/fold ([ht ht]) ([(k v) (in-hash one-ht)])
      (if (hash-ref ht k #f)
          ht
          (hash-set ht k (source->absolute-source i v))))))

(define (extract-pkg-dependencies get-info
                                  #:build-deps? [build-deps? #t]
                                  #:filter? [filter? #f]
                                  #:versions? [versions? #f])
  (define v (if get-info
                (get-info 'deps (lambda () empty))
                empty))
  ((check-dependencies 'deps) v)
  (define v2 (if (and get-info build-deps?)
                 (get-info 'build-deps (lambda () empty))
                 empty))
  ((check-dependencies 'build-deps) v2)
  (define all-v (append v v2))
  (if filter?
      (for/list ([dep (in-list all-v)]
                 #:when (dependency-this-platform? dep))
        (define name
          (if (pair? dep)
              (car dep)
              dep))
        (if versions?
            (list name (dependency->version dep))
            name))
      all-v))

(define (get-pkg-content desc 
                         #:namespace [metadata-ns (make-metadata-namespace)]
                         #:extract-info [extract-info extract-pkg-dependencies])
  (define-values (pkg-name dir cksum clean? module-paths) 
    (pkg-stage desc #:in-place? #t #:namespace metadata-ns))
  (define get-info (get-info/full dir #:namespace metadata-ns))
  (begin0
   (values cksum
           (set->list module-paths)
           (extract-info get-info))
   (when clean?
     (delete-directory/files dir))))

(define (pkg-directory->module-paths dir pkg-name 
                                     #:namespace [metadata-ns (make-metadata-namespace)])
  (set->list (directory->module-paths dir pkg-name metadata-ns)))

(define (directory->module-paths dir pkg-name metadata-ns)
  (define dummy (build-path dir "dummy.rkt"))
  (define compiled (string->path-element "compiled"))
  (define single-collect (pkg-single-collection dir #:name pkg-name #:namespace metadata-ns))
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
    (let loop ([s (set)] [f 'init] [check-zo? #f])
      (cond
       [(eq? f 'init)
        (for/fold ([s s]) ([f (directory-list)])
          (loop s f check-zo?))]
       [(directory-exists? f)
        ;; Count ".zo" files toward the set of module paths only
        ;; if an "info.rkt" in an enclosing directory says to
        ;; assume virtual sources. Otherwise, the ".zo" file will
        ;; be discarded by `raco setup'.
        (define sub-check-zo?
          (or check-zo?
              (let ([i (get-pkg-info f metadata-ns)])
                (and i
                     (i 'assume-virtual-sources (lambda () #f))))))
        (for/fold ([s s]) ([f (directory-list f #:build? #t)])
          (loop s f sub-check-zo?))]
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
           [(regexp-match? #rx#"[.](?:rkt|ss|scrbl)$" bstr)
            (try-path s f)]
           [(and check-zo?
                 (regexp-match? #rx#"_(?:rkt|ss|scrbl)[.]zo$" (path-element->bytes name)))
            (define-values (dir-base dir-name dir?) (split-path base))
            (cond
             [(eq? 'relative dir-base) s]
             [(equal? dir-name compiled)
              (define bstr2 (regexp-replace
                             #rx#"_(?:rkt|ss|scrbl)[.]zo$"
                             (path-element->bytes name)
                             #".rkt"))
              (if (equal? #"info.rkt" bstr2)
                  s
                  (try-path s (build-path dir-base
                                          (bytes->path-element
                                           bstr2))))]
             [else s])]
           [else s])])]))))

(define (pkg-directory->additional-installs dir pkg-name
                                            #:namespace [metadata-ns (make-metadata-namespace)]
                                            #:system-type [sys-type #f]
                                            #:system-library-subpath [sys-lib-subpath #f])
  (set->list (directory->additional-installs dir pkg-name metadata-ns
                                             #:system-type sys-type
                                             #:system-library-subpath sys-lib-subpath)))

(define (directory->additional-installs dir pkg-name metadata-ns
                                        #:system-type [sys-type #f]
                                        #:system-library-subpath [sys-lib-subpath #f])
  (define single-collect
    (pkg-single-collection dir #:name pkg-name #:namespace metadata-ns))
  (let loop ([s (set)] [f dir] [top? #t] [omits (set)])
    (cond
     [(and (directory-exists? f)
           (not (set-member? omits (simplify-path f))))
      (define i (get-pkg-info f metadata-ns))
      (define omit-paths (if i
                             (i 'compile-omit-paths (lambda () null))
                             null))
      (cond
       [(eq? omit-paths 'all)
        s]
       [else
        (define omit-files (if i
                               (i 'compile-omit-files (lambda () null))
                               null))
        (define new-s
          (if (and i (or single-collect (not top?)))
              (set-union (extract-additional-installs i sys-type sys-lib-subpath)
                         s)
              s))
        (define new-omits
          (set-union
           omits
           (for/set ([i (in-list (append omit-paths omit-files))])
             (simplify-path (build-path f i)))))
        (for/fold ([s new-s]) ([f (directory-list f #:build? #t)])
          (loop s f #f new-omits))])]
     [else s])))

(define (extract-additional-installs i sys-type sys-lib-subpath)
  (define (extract-documents i)
    (let ([s (i 'scribblings (lambda () null))])
      (for/set ([doc (in-list (if (list? s) s null))]
                #:when (and (list? doc)
                            (pair? doc)
                            (path-string? (car doc))
                            (or ((length doc) . < . 2)
                                (list? (cadr doc)))
                            (or ((length doc) . < . 4)
                                (collection-name-element? (list-ref doc 3)))))
        (define flags (if ((length doc) . < . 2)
                          null
                          (cadr doc)))
        (cond
         [(member 'main-doc-root flags) '(main-doc-root . "root")]
         [(member 'user-doc-root flags) '(user-doc-root . "root")]
         [else
          (cons 'doc
                (string-foldcase
                 (if ((length doc) . < . 4)
                     (let-values ([(base name dir?) (split-path (car doc))])
                       (path->string (path-replace-suffix name #"")))
                     (list-ref doc 3))))]))))
  (define (extract-paths i tag keys)
    (define (get k)
      (define l (i k (lambda () null)))
      (if (and (list? l) (andmap path-string? l))
          l
          null))
    (list->set (map (lambda (v) (cons tag
                                      (let-values ([(base name dir?) (split-path v)])
                                        ;; Normalize case, because some platforms
                                        ;; have case-insensitive filesystems:
                                        (string-foldcase (path->string name)))))
                    (apply
                     append
                     (for/list ([k (in-list keys)])
                       (get k))))))
  (define (extract-launchers i)
    (extract-paths i 'exe '(racket-launcher-names
                            mzscheme-launcher-names
                            gracket-launcher-names
                            mred-launcher-names)))
  (define (extract-foreign-libs i)
    (extract-paths i 'lib '(copy-foreign-libs
                            move-foreign-libs)))
  (define (extract-shared-files i)
    (extract-paths i 'share '(copy-shared-files
                              move-shared-files)))
  (define (extract-man-pages i)
    (extract-paths i 'man '(copy-man-pages
                            move-man-pages)))
  (define (this-platform? i)
    (define v (i 'install-platform (lambda () #rx"")))
    (or (not (platform-spec? v))
        (matching-platform? v
                            #:system-type sys-type
                            #:system-library-subpath sys-lib-subpath)))
  (set-union (extract-documents i)
             (extract-launchers i)
             (if (this-platform? i)
                 (set-union
                  (extract-foreign-libs i)
                  (extract-shared-files i)
                  (extract-man-pages i))
                 (set))))

(define (get-additional-installed kind skip-ht-keys ai-cache metadata-ns path-pkg-cache)
  (or (unbox ai-cache)
      (let ()
        (define skip-pkgs (list->set (hash-keys skip-ht-keys)))
        (define dirs (find-relevant-directories '(scribblings
                                                  racket-launcher-names
                                                  mzscheme-launcher-names
                                                  gracket-launcher-names
                                                  mred-launcher-names
                                                  copy-foreign-libs
                                                  move-foreign-libs
                                                  copy-shared-files
                                                  move-shared-files
                                                  copy-man-pages
                                                  move-man-pages)
                                                (if (eq? 'user (current-pkg-scope))
                                                    'all-available
                                                    'no-user)))
        (define s (for/fold ([s (set)]) ([dir (in-list dirs)])
                    (cond
                     [(set-member? skip-pkgs (path->pkg dir #:cache path-pkg-cache))
                      s]
                     [else
                      (define i (get-pkg-info dir metadata-ns))
                      (if i
                          (set-union s (extract-additional-installs i #f #f))
                          s)])))
        (set-box! ai-cache s)
        s)))

(define (pkg-catalog-update-local #:catalogs [catalogs (pkg-config-catalogs)]
                                  #:set-catalogs? [set-catalogs? #t]
                                  #:catalog-file [catalog-file (db:current-pkg-catalog-file)]
                                  #:quiet? [quiet? #f]
                                  #:consult-packages? [consult-packages? #f]
                                  #:skip-download-failures? [skip-download-failures? #f])
  (parameterize ([db:current-pkg-catalog-file catalog-file])
    (define current-catalogs (db:get-catalogs))
    (cond
     [set-catalogs?
      (unless (equal? catalogs current-catalogs)
        (db:set-catalogs! catalogs))]
     [else
      (unless (for/and ([catalog (in-list catalogs)])
                (member catalog current-catalogs))
        (error 'pkg-catalog-update-local
               (~a "given catalog list is not a superset of recorded catalogs\n"
                   "  given: ~s\n"
                   "  recorded: ~s")
               catalogs
               current-catalogs))])

    (for ([catalog (in-list catalogs)])
      (unless quiet?
        (printf/flush "Updating from ~a\n" catalog))
      (parameterize ([current-pkg-catalogs (list (string->url catalog))])
        (define details (for/hash ([(name ht) (get-all-pkg-details-from-catalogs)])
                          (values name (select-info-version ht))))
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
          (define tags (hash-ref ht 'tags #f))
          (when tags
            (db:set-pkg-tags! name catalog tags))
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
              (printf/flush "Downloading ~s\n" source))
            (define-values (checksum modules deps)
              (get-pkg-content (pkg-desc source
                                         #f
                                         name
                                         (hash-ref ht 'checksum #f)
                                         #f)))
            (db:set-pkg-modules! name catalog checksum modules)
            (db:set-pkg-dependencies! name catalog checksum deps)))))))

(define (pkg-catalog-archive dest-dir
                             src-catalogs
                             #:from-config? [from-config? #f]
                             #:state-catalog [state-catalog #f]
                             #:relative-sources? [relative-sources? #f]
                             #:quiet? [quiet? #f]
                             #:package-exn-handler [package-exn-handler (lambda (name exn) (raise exn))])
  (when (and state-catalog
             (not (db-path? (if (path? state-catalog)
                                state-catalog
                                (string->path state-catalog)))))
    (pkg-error (~a "bad state file path\n"
                   "  given: ~a\n"
                   "  expected: path with \".sqlite\" extension")
               state-catalog))
  ;; Take a snapshot of the source catalog:
  (define temp-catalog-file (make-temporary-file "pkg~a.sqlite"))
  (pkg-catalog-copy (map url-or-path->url-string
                         (map src->url-or-path src-catalogs))
                    temp-catalog-file
                    #:force? #t ; replaces temporary file
                    #:from-config? from-config?)
  (define pkgs
    (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
      (db:get-pkgs)))
  ;; Reset state catalog to new packages:
  (when state-catalog
    (parameterize ([db:current-pkg-catalog-file state-catalog])
      (db:set-catalogs! '("local"))
      (db:set-pkgs! "local" (map db:pkg-name pkgs))))
  ;; Remove any package not in `pkgs`:
  (define pkgs-dir (build-path dest-dir "pkgs"))
  (when (directory-exists? pkgs-dir)
    (define keep-pkgs (list->set (map db:pkg-name pkgs)))
    (for ([f (in-list (directory-list pkgs-dir))])
      (cond
       [(regexp-match #rx"^(.*)[.]zip(?:[.]CHECKSUM)?$" f)
        => (lambda (m)
             (unless (set-member? keep-pkgs (cadr m))
               (unless quiet?
                 (printf/flush "Removing old package file ~a\n" f))
               (delete-file (build-path pkgs-dir f))))])))
  ;; Check on each new package:
  (for ([pkg (in-list (sort pkgs string<? #:key db:pkg-name))])
    (define name (db:pkg-name pkg))
    (with-handlers ([exn:fail? (lambda (exn)
                                 (package-exn-handler name exn))])
      (define current-checksum (and state-catalog
                                    (parameterize ([db:current-pkg-catalog-file state-catalog])
                                      (define l (db:get-pkgs #:name (db:pkg-name pkg)))
                                      (and (= 1 (length l))
                                           (db:pkg-checksum (car l))))))
      (define pkg-file (build-path dest-dir "pkgs" (format "~a.zip" name)))
      (define pkg-checksum-file (path-replace-suffix pkg-file #".zip.CHECKSUM"))
      (unless (and current-checksum
                   (equal? current-checksum (db:pkg-checksum pkg))
                   (file-exists? pkg-file)
                   (file-exists? pkg-checksum-file)
                   (equal? (file->string pkg-checksum-file)
                           (call-with-input-file* pkg-file sha1)))
        (unless quiet?
          (printf/flush "== Archiving ~a ==\nchecksum: ~a\n" (db:pkg-name pkg) (db:pkg-checksum pkg)))
        ;; Download/unpack existing package:
        (define-values (staged-name staged-dir staged-checksum clean? staged-mods)
          (pkg-stage
           (pkg-desc (db:pkg-source pkg) #f (db:pkg-name pkg) (db:pkg-checksum pkg) #f)
           #:in-place? #t
           #:use-cache? #t
           #:quiet? quiet?))
        (make-directory* (build-path dest-dir "pkgs"))
        ;; Repack:
        (pkg-create 'zip
                    staged-dir
                    #:pkg-name name
                    #:dest (build-path dest-dir "pkgs")
                    #:quiet? quiet?)
        (when clean? (delete-directory/files staged-dir))
        ;; Record packed result:
        (when state-catalog
          (parameterize ([db:current-pkg-catalog-file state-catalog])
            (db:set-pkg! name "local"
                         (db:pkg-author pkg)
                         (db:pkg-source pkg)
                         staged-checksum
                         (db:pkg-desc pkg)))))
      ;; Record packed result:
      (define new-checksum (file->string pkg-checksum-file))
      (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
        (define modules (db:get-pkg-modules name (db:pkg-catalog pkg) (db:pkg-checksum pkg)))
        (define dependencies (db:get-pkg-dependencies name (db:pkg-catalog pkg) (db:pkg-checksum pkg)))
        (db:set-pkg! name (db:pkg-catalog pkg)
                     (db:pkg-author pkg)
                     (path->string (path->complete-path pkg-file))
                     new-checksum
                     (db:pkg-desc pkg))
        (db:set-pkg-modules! name (db:pkg-catalog pkg)
                             new-checksum
                             modules)
        (db:set-pkg-dependencies! name (db:pkg-catalog pkg)
                                  new-checksum
                                  dependencies))))
  (define dest-catalog (build-path dest-dir "catalog"))
  (unless quiet?
    (printf/flush "Creating catalog ~a\n" dest-catalog))
  (pkg-catalog-copy (list temp-catalog-file)
                    (build-path dest-dir "catalog")
                    #:force? #t
                    #:override? #t
                    #:relative-sources? relative-sources?)
  (delete-file temp-catalog-file))

(define (choose-catalog-file)
  (define default (db:current-pkg-catalog-file))
  (if (file-exists? default)
      default
      (let ([installation (build-path (find-share-dir) "pkgs" (file-name-from-path default))])
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
  (or/c 'installation 'user
        (and/c path? complete-path?)))

(provide
 (all-from-out "path.rkt")
 with-pkg-lock
 with-pkg-lock/read-only
 pkg-desc?
 (contract-out
  [current-pkg-scope
   (parameter/c package-scope/c)]
  [current-pkg-scope-version
   (parameter/c string?)]
  [current-pkg-lookup-version
   (parameter/c string?)]
  [current-pkg-error 
   (parameter/c procedure?)]
  [current-pkg-catalogs
   (parameter/c (or/c #f (listof url?)))]
  [current-pkg-download-cache-dir
   (parameter/c (or/c #f (and path-string? complete-path?)))]
  [current-pkg-download-cache-max-files
   (parameter/c (or/c #f real?))]
  [current-pkg-download-cache-max-bytes
   (parameter/c (or/c #f real?))]
  [pkg-directory
   (-> string? (or/c path-string? #f))]
  [pkg-desc 
   (-> string? 
       (or/c #f 'file 'dir 'link 'static-link 'file-url 'dir-url 'github 'name) 
       (or/c string? #f)
       (or/c string? #f)
       boolean?
       pkg-desc?)]
  [pkg-config
   (->* (boolean? (listof string?))
        (#:from-command-line? boolean?)
        void?)]
  [pkg-create
   (->* ((or/c 'zip 'tgz 'plt 'MANIFEST)
         path-string?)
        (#:source (or/c 'dir 'name)
                  #:pkg-name (or/c #f string?)
                  #:mode (or/c 'as-is 'source 'binary 'built)
                  #:quiet? boolean?
                  #:from-command-line? boolean?
                  #:dest (or/c (and/c path-string? complete-path?) #f))
        void?)]
  [pkg-update
   (->* ((listof (or/c string? pkg-desc?)))
        (#:dep-behavior dep-behavior/c
                        #:all? boolean?
                        #:update-deps? boolean?
                        #:update-implies? boolean?
                        #:quiet? boolean?
                        #:from-command-line? boolean?
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:use-cache? boolean?
                        #:strip (or/c #f 'source 'binary)
                        #:link-dirs? boolean?)
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-remove
   (->* ((listof string?))
        (#:auto? boolean?
                 #:force? boolean?
                 #:quiet? boolean?
                 #:from-command-line? boolean?
                 #:demote? boolean?)
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-show
   (->* (string?)
        (#:directory? boolean?
                      #:auto? boolean?)
        void?)]
  [pkg-install
   (->* ((listof pkg-desc?))
        (#:dep-behavior dep-behavior/c
                        #:update-deps? boolean?
                        #:update-implies? boolean?
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:use-cache? boolean?
                        #:skip-installed? boolean?
                        #:quiet? boolean?
                        #:from-command-line? boolean?
                        #:strip (or/c #f 'source 'binary)
                        #:link-dirs? boolean?)
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
  [pkg-migrate
   (->* (string?)
        (#:dep-behavior dep-behavior/c
                        #:all-platforms? boolean?
                        #:force? boolean?
                        #:ignore-checksums? boolean?
                        #:strict-doc-conflicts? boolean?
                        #:use-cache? boolean?
                        #:quiet? boolean?
                        #:from-command-line? boolean?
                        #:strip (or/c #f 'source 'binary))
        (or/c #f 'skip (listof (or/c path-string? (non-empty-listof path-string?)))))]
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
                        #:override? boolean?
                        #:relative-sources? boolean?)
        void?)]
  [pkg-catalog-archive
   (->* (path-string? (listof string?))
        (#:from-config? boolean?
                        #:state-catalog (or/c path-string? #f)
                        #:relative-sources? boolean?
                        #:quiet? boolean?
                        #:package-exn-handler (string? exn:fail? . -> . any))
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
                  (#:namespace namespace?
                               #:in-place? boolean?
                               #:strip (or/c #f 'source 'binary)
                               #:use-cache? boolean?
                               #:quiet? boolean?)
                  (values string?
                          path?
                          (or/c #f string?)
                          boolean?
                          (listof module-path?)))]
  [pkg-config-catalogs
   (-> (listof string?))]
  [pkg-catalog-update-local
   (->* ()
        (#:catalogs (listof string?)
         #:set-catalogs? boolean?
         #:catalog-file path-string?
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
                            any/c)
                        #:namespace namespace?)
        (values (or/c #f string?)
                (listof module-path?)
                any/c))]
  [extract-pkg-dependencies
   (->* ((symbol? (-> any/c) . -> . any/c))
        (#:build-deps? boolean?
                       #:filter? boolean?
                       #:versions? boolean?)
        (listof (or/c string? (cons/c string? list?))))]
  [pkg-single-collection
   (->* (path-string?)
        (#:name string?
                #:namespace namespace?)
        (or/c #f string?))]
  [find-pkg-installation-scope (->* (string?)
                                    (#:next? boolean?)
                                    (or/c #f package-scope/c))]
  [pkg-directory->module-paths (->* (path-string? string?)
                                    (#:namespace namespace?)
                                    (listof module-path?))]
  [pkg-directory->additional-installs (->* (path-string? string?)
                                           (#:namespace namespace?
                                                        #:system-type (or/c #f symbol?)
                                                        #:system-library-subpath (or/c #f path?))
                                           (listof (cons/c symbol? string?)))]))
