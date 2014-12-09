#lang racket/base
(require net/url
         racket/path
         racket/format
         racket/port
         (prefix-in db: "../db.rkt")
         "../name.rkt"
         "params.rkt"
         "config.rkt"
         "print.rkt")

(provide select-info-version
         source->relative-source
         package-catalog-lookup
         package-catalog-lookup-source
         get-all-pkg-names-from-catalogs
         get-pkg-details-from-catalogs
         get-all-pkg-details-from-catalogs
         db-path?)

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

(define (package-catalog-lookup pkg details? cache download-printf)
  (when (and details? cache)
    (error "internal error: catalog-lookup cache doesn't keep details"))
  (or
   (and cache
        (hash-ref cache pkg #f))
   (add-to-cache
    pkg cache
    (for/or ([i (in-list (pkg-catalogs))])
      (define (consulting-catalog suffix)
        (if download-printf
            (download-printf "Resolv~a ~s via ~a\n" suffix pkg (url->string i))
            (log-pkg-debug "consult~a catalog ~a" suffix (url->string i))))
      (source->absolute-source
       i
       (select-info-version
        (catalog-dispatch
         i
         ;; Server:
         (lambda (i)
           (consulting-catalog "ing")
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
                (begin
                  (consulting-catalog "ed")
                  (db-pkg-info (car pkgs) details?))))
         ;; Local directory:
         (lambda (path)
           (define pkg-path (build-path path "pkg" pkg))
           (and (file-exists? pkg-path)
                (begin
                  (consulting-catalog "ed")
                  (call-with-input-file* pkg-path read)))))))))
   (pkg-error (~a "cannot find package on catalogs\n"
                  "  package: ~a")
              pkg)))

(define (package-catalog-lookup-source pkg cache download-printf)
  (hash-ref (package-catalog-lookup pkg #f cache download-printf)
            'source))

(define (add-to-cache pkg cache v)
  (when (and cache v)
    (hash-set! cache pkg v))
  v)

(define (read-from-server who url pred
                          [failure
                           (lambda (s)
                             (error who
                                    (~a "bad response from server\n"
                                        "  url: ~a\n"
                                        "  response: ~v")
                                    (url->string url)
                                    s))])
  (define bytes (call-with-url url port->bytes))
  ((if bytes
     (with-handlers ([exn:fail:read? (lambda (exn)
                                       (lambda () (failure bytes)))])
       (define v (read (open-input-bytes bytes)))
       (lambda ()
         (if (pred v)
           v
           (failure bytes))))
     (lambda () (failure #f)))))

;; uses a custodian to avoid leaks:
(define (call-with-url url handler)
  (define c (make-custodian))
  (dynamic-wind
      void
      (lambda ()
        (define-values (p hs)
          (parameterize ([current-custodian c])
            (get-pure-port/headers url #:redirections 25 #:status? #t)))
        (begin0
          (and (string=? "200" (substring hs 9 12))
               (handler p))
          (close-input-port p)))
      (lambda ()
        (custodian-shutdown-all c))))

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
    (package-catalog-lookup name #t #f #f)))

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
