#lang racket/base
(require setup/dirs
         racket/format
         racket/match
         racket/set
         "../path.rkt"
         "params.rkt"
         "lock.rkt"
         "print.rkt"
         "dirs.rkt"
         "config.rkt")

;; Read and writing the database of installed packages.

(provide read-pkg-db
         merge-pkg-dbs
         find-pkg-installation-scope
         package-info
         update-pkg-db!
         remove-from-pkg-db!
         pkg-directory
         pkg-directory*
         pkg-directory**
         make-pkg-info
         update-auto
         scope->links-file
         installed-pkg-table
         installed-pkg-names)

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

(define (package-info pkg-name [fail? #t]
                      #:db [given-db #f]
                      #:cache [cache #f])
  (define db (or given-db
                 (and cache
                      (hash-ref cache (current-pkg-scope) #f))
                 (let ([db (read-pkg-db)])
                   (when cache
                     (hash-set! cache (current-pkg-scope) db))
                   db)))
  (define pi (hash-ref db pkg-name #f))
  (cond
    [pi
     pi]
    [(not fail?)
     #f]
    [else
     (pkg-not-installed pkg-name db)]))

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

;; return the current scope as a string
(define (current-scope->string)
  (define scope (current-pkg-scope))
  (cond
   [(path? scope) (path->string scope)]
   [else (symbol->string scope)]))

;; ----------------------------------------

(define (update-pkg-db! pkg-name info)
  (write-file-hash!
   (pkg-db-file)
   (hash-set (read-pkg-db) pkg-name info)))

(define (remove-from-pkg-db! pkg-name)
  (write-file-hash!
   (pkg-db-file)
   (hash-remove (read-pkg-db) pkg-name)))

;; ----------------------------------------

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

(define (pkg-directory pkg-name #:cache [cache #f])
  ;; Warning: takes locks individually.
  (pkg-directory** pkg-name
                   #:cache cache
                   (lambda (f)
                     (with-pkg-lock/read-only
                      (f)))))

(define (pkg-directory** pkg-name [call-with-pkg-lock (lambda (f) (f))]
                         #:cache [cache #f])
  (for/or ([scope (in-list (get-scope-list))])
    (parameterize ([current-pkg-scope scope])
      (call-with-pkg-lock
       (lambda ()
         (pkg-directory* pkg-name #:cache cache))))))

(define (pkg-directory* pkg-name
                        #:db [db #f]
                        #:cache [cache #f])
  (define info (package-info pkg-name #f #:db db #:cache cache))
  (and info
       (let ()
         (match-define (pkg-info orig-pkg checksum _) info)
         (match orig-pkg
           [`(,(or 'link 'static-link 'clone) ,orig-pkg-dir . ,_)
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

;; ----------------------------------------

(define (installed-pkg-table #:scope [given-scope #f])
  (parameterize ([current-pkg-scope 
                  (or given-scope (default-pkg-scope))])
    (with-pkg-lock/read-only
     (read-pkg-db))))

(define (installed-pkg-names #:scope [given-scope #f])
  (sort (hash-keys (installed-pkg-table #:scope given-scope))
        string-ci<=?))
