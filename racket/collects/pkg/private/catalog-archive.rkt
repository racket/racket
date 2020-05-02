#lang racket/base
(require racket/format
         racket/file
         racket/set
         openssl/sha1
         "../name.rkt"
         (prefix-in db: "../db.rkt")
         "catalog.rkt"
         "catalog-copy.rkt"
         "print.rkt"
         "stage.rkt"
         "desc.rkt"
         "create.rkt"
         "path.rkt"
         "dep.rkt")

(provide pkg-catalog-archive)

(define (pkg-catalog-archive dest-dir
                             src-catalogs
                             #:include [include-names #f]
                             #:include-deps? [include-deps? #f]
                             #:include-deps-sys+subpath [include-deps-sys+subpath #f]
                             #:exclude [exclude-names '()]
                             #:from-config? [from-config? #f]
                             #:state-catalog [state-catalog #f]
                             #:relative-sources? [relative-sources? #f]
                             #:quiet? [quiet? #f]
                             #:fast-file-copy? [fast-file-copy? #f]
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
    (filter-pkgs
     (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
       (db:get-pkgs))
     include-names include-deps? include-deps-sys+subpath
     (cons "racket" exclude-names)
     temp-catalog-file))
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
      (define pkg-checksum-file (path-replace-extension pkg-file #".zip.CHECKSUM"))
      (unless (and current-checksum
                   (equal? current-checksum (db:pkg-checksum pkg))
                   (file-exists? pkg-file)
                   (file-exists? pkg-checksum-file)
                   (equal? (file->string pkg-checksum-file)
                           (call-with-input-file* pkg-file sha1)))
        (unless quiet?
          (printf/flush "== Archiving ~a ==\nchecksum: ~a\n" (db:pkg-name pkg) (db:pkg-checksum pkg)))
        (define-values (staged-checksum)
          (cond
            [(and fast-file-copy?
                  (let ([src (db:pkg-source pkg)])
                    (let-values ([(name type) (package-source->name+type src #f)])
                      (case type
                        [(file) (package-source->path src 'file)]
                        [else #f]))))
             => (lambda (path)
                  (define-values (base filename dir) (split-path path))
                  (make-directory* (build-path dest-dir "pkgs"))
                  (copy-file path (build-path dest-dir "pkgs" filename) #t)
                  (copy-file (path-add-checksum-suffix path)
                             (build-path dest-dir "pkgs" (path-add-checksum-suffix filename))
                             #t)
                  (values (db:pkg-checksum pkg)))]
            [else
             ;; Download/unpack existing package:
             (define-values (staged-name staged-dir staged-checksum clean? staged-mods)
               (pkg-stage
                (pkg-desc (db:pkg-source pkg) #f (db:pkg-name pkg) (db:pkg-checksum pkg) #f #f)
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
             (values staged-checksum)]))
        ;; Record packed result in state catalog:
        (when state-catalog
          (parameterize ([db:current-pkg-catalog-file state-catalog])
            (db:set-pkg! name "local"
                         (db:pkg-author pkg)
                         (db:pkg-source pkg)
                         staged-checksum
                         (db:pkg-desc pkg)))))
      ;; Record packed result at new location:
      (define new-checksum (file->string pkg-checksum-file))
      (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
        (db:call-with-pkgs-transaction
         (lambda ()
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
                                     dependencies))))))
  (define dest-catalog (build-path dest-dir "catalog"))
  (unless quiet?
    (printf/flush "Creating catalog ~a\n" dest-catalog))
  (pkg-catalog-copy (list temp-catalog-file)
                    (build-path dest-dir "catalog")
                    #:force? #t
                    #:override? #t
                    #:relative-sources? relative-sources?
                    #:include-only (map db:pkg-name pkgs))
  (delete-file temp-catalog-file))

;; ----------------------------------------

(define (filter-pkgs pkgs include-names include-deps? include-deps-sys+subpath exclude-names catalog)
  (cond
    [(not include-names)
     (if (null? exclude-names)
         pkgs
         (for/list ([pkg (in-list pkgs)]
                    #:unless (member (db:pkg-name pkg) exclude-names))
           pkg))]
    [else
     (define pkg-map (for/hash ([pkg (in-list pkgs)])
                       (values (db:pkg-name pkg) pkg)))
     (define include-table
       (let loop ([include-table #hash()]
                  [todo-names include-names])
         (cond
           [(null? todo-names) include-table]
           [else
            (let ([name (car todo-names)])
              (cond
                [(or (hash-ref include-table name #f)
                     (member name exclude-names))
                 (loop include-table (cdr todo-names))]
                [else
                 (loop (hash-set include-table name #t)
                       (append
                        (if include-deps?
                            (let ([pkg (hash-ref pkg-map name #f)])
                              (cond
                                [(not pkg)
                                 ;; Skip a missing dependency
                                 null]
                                [else
                                 (for/list ([dep (in-list
                                                  (parameterize ([db:current-pkg-catalog-file catalog])
                                                    (db:get-pkg-dependencies name
                                                                             (db:pkg-catalog pkg)
                                                                             (db:pkg-checksum pkg))))]
                                            #:when (or (not include-deps-sys+subpath)
                                                       (dependency-for-platform? dep include-deps-sys+subpath)))
                                   (car dep))]))
                            null)
                        (cdr todo-names)))]))])))
     (for/list ([pkg (in-list pkgs)]
                #:when (hash-ref include-table (db:pkg-name pkg) #f))
       pkg)]))
