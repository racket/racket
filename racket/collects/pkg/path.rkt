#lang racket/base
(require setup/dirs)

(provide (struct-out pkg-info)
         (struct-out pkg-info/alt)
         (struct-out sc-pkg-info)
         (struct-out sc-pkg-info/alt)
         get-pkgs-dir
         read-pkgs-db
         read-pkg-file-hash
         path->pkg
         path->pkg+subpath
         path->pkg+subpath+scope
         path->pkg+subpath+collect
         path->pkg+subpath+collect+scope)

(struct pkg-info (orig-pkg checksum auto?) #:prefab)
(struct pkg-info/alt pkg-info (dir-name) #:prefab) ; alternate installation directory
(struct sc-pkg-info pkg-info (collect) #:prefab) ; a pkg with a single collection
(struct sc-pkg-info/alt sc-pkg-info (dir-name) #:prefab) ; alternate installation

(define (check-scope who scope)
  (unless (or (eq? scope 'user)
              (eq? scope 'installation)
              (and (path? scope)
                   (complete-path? scope)))
    (raise-argument-error 
     who 
     "(or/c 'user 'installation (and/c path? complete-path?))"
     scope)))

(define (get-pkgs-dir scope [user-version (get-installation-name)])
  (check-scope 'get-pkgs-dir scope)
  (unless (string? user-version)
    (raise-argument-error 'get-pkgs-dir "string?" user-version))
  (if (path? scope)
      scope
      (case scope
        [(installation) (find-pkgs-dir)]
        [(user) (find-user-pkgs-dir user-version)]
        [else (error "unknown package scope")])))

(define (read-pkg-file-hash file)
  (with-handlers ([exn:fail? (lambda (x) 
                               (log-error (string-append
                                           "error reading package file hash\n"
                                           "  error: ~s")
                                          (exn-message x))
                               (hash))])
    (if (file-exists? file) ; don't complain if the file is missing
        (call-with-input-file*
         file
         (lambda (i)
           (call-with-default-reading-parameterization
            (lambda ()
              (define ht (read i))
              (unless (hash? ht) (error "content is not a hash"))
              ht))))
        (hash))))

(define (read-pkgs-db scope [user-version (get-installation-name)])
  (check-scope 'read-pkgs-db scope)
  (let ([db (read-pkg-file-hash 
             (build-path (get-pkgs-dir scope user-version) "pkgs.rktd"))])
    ;; compatibility: map 'pnr to 'catalog:
    (for/hash ([(k v) (in-hash db)])
      (values k
              (if (eq? 'pnr (car (pkg-info-orig-pkg v)))
                  ;; note: legacy 'pnr entry cannot be a single-collection package
                  (struct-copy pkg-info v
                               [orig-pkg `(catalog ,(cadr (pkg-info-orig-pkg v)))])
                  v)))))

(define (path->pkg+subpath+collect* who given-p cache want-collect?)
  (unless (path-string? given-p)
    (raise-argument-error who "path-string?" given-p))
  (unless (or (not cache)
              (and (hash? cache)
                   (not (immutable? cache))))
    (raise-argument-error who "(or/c #f (and/c hash? (not/c immutable?)))" cache))
  (define (explode p)
    (explode-path
     (simplify-path (path->complete-path p))))
  (define (sub-path? < p d)
    (and ((length d) . < . (length p))
         (for/and ([de (in-list d)]
                   [pe (in-list p)])
           (equal? (normal-case-path de) (normal-case-path pe)))))
  (define p (explode given-p))
  (define (build-path* l)
    (if (null? l) 'same (apply build-path l)))
  (for/fold ([pkg #f] [subpath #f] [collect #f] [install-scope #f])
      ([scope (in-list (list* 'user
                              (get-pkgs-search-dirs)))]
       #:when (not pkg))
    (define d (or (and cache
                       (hash-ref cache `(dir ,scope) #f))
                  (let ([d (explode (get-pkgs-dir scope))])
                    (when cache (hash-set! cache `(dir ,scope) d))
                    d)))
    (define (read-pkg-db/cached)
      (or (and cache
               (hash-ref cache `(db ,scope) #f))
          (let ([db (read-pkgs-db scope)])
            (when cache (hash-set! cache `(db ,scope) db))
            db)))
    (define (normal-case-mapping/cached db)
      (if (eq? 'windows (system-path-convention-type))
	  (or (and cache
		   (hash-ref cache `(normal-case ,scope) #f))
	      (let ([ht (for/hash ([n (in-hash-keys db)])
			  (values (normal-case-path n) n))])
		(when cache (hash-set! cache `(normal-case ,scope) ht))
		ht))
	  #hash()))
    (cond
     [(sub-path? < p d)
      ;; Under the installation mode's package directory.
      ;; We assume that no one else writes there, so the
      ;; next path element is the package name, the package
      ;; name followed by "+<n>", or ".trash"
      (define len (length d))
      (define pkg-name (path-element->string (list-ref p len)))
      (if (or (regexp-match? #rx"pkgs[.]rktd" pkg-name)
              (regexp-match? #rx"[.]trash" pkg-name))
          (values #f #f #f #f) ; don't count the database or trash can as a package
	  (let* ([pkg-name
		  (if (regexp-match? #rx"[+]" pkg-name) ; +<n> used as an alternate path, sometimes
		      (regexp-replace #rx"[+].*$" pkg-name "")
		      pkg-name)]
		 [db (read-pkg-db/cached)]
		 ;; Double-check that such a package is installed, in case
		 ;; there's a junk directory or the path case is wrong:
		 [pkg-name (if (hash-ref db pkg-name #f)
			       pkg-name
			       (hash-ref (normal-case-mapping/cached db)
					 (normal-case-path pkg-name)
					 #f))])
	    (if pkg-name
		(values pkg-name
			(build-path* (list-tail p (add1 len)))
			(and want-collect?
			     (let ([i (hash-ref (read-pkg-db/cached) pkg-name #f)])
			       (and i (sc-pkg-info? i) (sc-pkg-info-collect i))))
			scope)
		(values #f #f #f #f))))]
     [else
      ;; Maybe it's a linked package
      (define pkgs-dir (get-pkgs-dir scope))
      (for/fold ([pkg #f] [subpath #f] [collect #f] [install-scope #f])
          ([(k v) (in-hash (read-pkg-db/cached))]
           #:when (not pkg))
        (define orig (pkg-info-orig-pkg v))
        (if (and (pair? orig)
                 (or (eq? 'link (car orig))
                     (eq? 'static-link (car orig))
                     (eq? 'clone (car orig))))
            (let ([e (or (and cache
                              (hash-ref cache `(pkg-dir ,(cadr orig)) #f))
                         (let ([e (explode (simplify-path 
                                            (path->complete-path (cadr orig) pkgs-dir) 
                                            #f))])
                           (when cache
                             (hash-set! cache `(pkg-dir ,(cadr orig)) e))
                           e))])
              (if (sub-path? <= p e)
                  (values k
                          (build-path* (list-tail p (length e)))
                          (and (sc-pkg-info? v) (sc-pkg-info-collect v))
                          scope)
                  (values #f #f #f #f)))
            (values #f #f #f #f)))])))

(define (path->pkg+subpath+collect+scope given-p #:cache [cache #f])
  (path->pkg+subpath+collect* 'path->pkg+subpath+collect given-p cache #t))

(define (path->pkg+subpath+collect given-p #:cache [cache #f])
  (define-values (pkg subpath collect scope)
    (path->pkg+subpath+collect* 'path->pkg+subpath+collect given-p cache #t))
  (values pkg subpath collect))

(define (path->pkg+subpath+scope given-p #:cache [cache #f])
  (define-values (pkg subpath collect scope)
    (path->pkg+subpath+collect* 'path->pkg+subpath given-p cache #f))
  (values pkg subpath scope))

(define (path->pkg+subpath given-p #:cache [cache #f])
  (define-values (pkg subpath collect scope)
    (path->pkg+subpath+collect* 'path->pkg+subpath given-p cache #f))
  (values pkg subpath))

(define (path->pkg given-p #:cache [cache #f])
  (define-values (pkg subpath collect scope)
    (path->pkg+subpath+collect* 'path->pkg given-p cache #f))
  pkg)
