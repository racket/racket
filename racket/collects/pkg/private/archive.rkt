#lang racket/base
(require racket/file
         racket/set
         racket/match
         racket/list
         setup/getinfo
         "../path.rkt"
         (prefix-in db: "../db.rkt")
         "dirs.rkt"
         "pkg-db.rkt"
         "params.rkt"
         "print.rkt"
         "desc.rkt"
         "stage.rkt"
         "create.rkt"
         "catalog-copy.rkt")

(provide pkg-archive-pkgs)

(define (pkg-archive-pkgs dest-dir pkg-names
                          #:include-deps? [include-deps? #f]
                          #:exclude [exclude null]
                          #:relative-sources? [relative-sources? #f]
                          #:quiet? [quiet? #f]
                          #:package-exn-handler [package-exn-handler
                                                         (λ (name exn) (raise exn))])
  (struct pkg (deps build-deps) #:transparent)
  (define (extract-pkg p) (if (string? p) p (car p)))

  (define (add-package-from-dir src-f f-name pkgs)
    (define i (get-info/full src-f))
    (cond
      [i
       (hash-set pkgs f-name (pkg (map extract-pkg (i 'deps (lambda () null)))
                                  (map extract-pkg (i 'build-deps (lambda () null)))))]
      [else pkgs]))

  (define unfiltered-pkgs
    (for/fold ([pkgs (hash)]) ([pkg-scope (in-list (get-all-pkg-scopes))])
      (define pkg-names (installed-pkg-names #:scope pkg-scope))
      (parameterize ([current-pkg-scope pkg-scope])
        (for/fold ([pkgs pkgs]) ([pkg (in-list pkg-names)])
          (define dir (pkg-directory pkg))
          (cond [dir (add-package-from-dir dir pkg pkgs)]
                [else pkgs])))))

  (define exclude+ (list* "base" "racket" exclude))

  (for ([p (in-list pkg-names)])
    (unless (hash-ref unfiltered-pkgs p #f)
      (pkg-error "cannot archive package \"~a\" because it is not installed" p)))

  ;; Filter to roots:
  (define pkgs/deps
    (cond
     [(not include-deps?)
      (for/hash ([(k v) (in-hash unfiltered-pkgs)]
                 #:when (member k pkg-names))
        (values k v))]
     [else
      (define seen (make-hash))
      (define (loop pkg)
        (cond
          [(member pkg exclude+)  (void)]
          [(hash-ref seen pkg #f) (void)]
          [else
           (define p (hash-ref unfiltered-pkgs pkg #f))
           (when p
             (hash-set! seen pkg #t)
             (for-each loop (pkg-deps p))
             (for-each loop (pkg-build-deps p)))]))
      (for-each loop pkg-names)
      (for/hash ([(k v) (in-hash unfiltered-pkgs)]
                 #:when (hash-ref seen k #f))
        (values k v))]))

  (define all-pkg-names (hash-keys pkgs/deps))

  ;; The temporary catalog we'll create, simulating the current install
  (define temp-catalog-file (make-temporary-file "pkg~a.sqlite"))
  ;; all the current installed packages
  (define all-installed-pkgs
    (for*/hash ([scope (in-list (get-all-pkg-scopes))]
                [(k v) (in-hash (read-pkgs-db scope))])
      (values k v)))

  ;; get the pkg descriptions we want
  (define pkgs
    (for/hash ([p (in-list all-pkg-names)])
      (values p
              (hash-ref all-installed-pkgs p
                        (λ _ (pkg-error
                              "cannot archive package \"~a\" because it is not installed" p))))))

  ;; set up temporary catalog with the right packages
  (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
    (db:set-catalogs! '("local"))
    (db:set-pkgs! "local" all-pkg-names))

  ;; Remove any package not in `pkgs`:
  (define pkgs-dir (build-path dest-dir "pkgs"))
  (when (directory-exists? pkgs-dir)
    (define keep-pkgs (list->set all-pkg-names))
    (for ([f (in-list (directory-list pkgs-dir))])
      (cond
       [(regexp-match #rx"^(.*)[.]zip(?:[.]CHECKSUM)?$" f)
        => (lambda (m)
             (unless (set-member? keep-pkgs (cadr m))
               (unless quiet?
                 (printf/flush "Removing old package file ~a\n" f))
               (delete-file (build-path pkgs-dir f))))])))

  (define (pkg->deps p)
    (match-define (pkg deps build-deps) (hash-ref pkgs/deps p))
    ;; NOTE: This include deps that don't get archived. It's not
    ;;       obvious which is the right decision but I've gone with
    ;;       including them since for "base" keeping but not archiving
    ;;       seems like the right choice.
    (remove-duplicates (append deps build-deps)))

  ;; Check on each new package:
  (for ([(name pkg-i) (in-hash pkgs)])
    (match-define (pkg-info _ checksum _) pkg-i)
    (with-handlers ([exn:fail? (λ (exn) (package-exn-handler name exn))])
      (define pkg-file (build-path dest-dir "pkgs" (format "~a.zip" name)))
      (define pkg-checksum-file (path-replace-suffix pkg-file #".zip.CHECKSUM"))
      (define pkg-dir (pkg-directory name))

      (unless pkg-dir
        (pkg-error "no directory found for package \"~a\"" name))

      (unless quiet?
        (printf/flush "== Archiving ~a ==\nchecksum: ~a\n" name checksum))
      ;; Download/unpack existing package:
      (define-values (staged-name staged-dir staged-checksum clean? staged-mods)
        (pkg-stage
         (pkg-desc (path->string pkg-dir) 'dir name checksum #f #f)
         #:in-place? #f
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
      (define new-checksum (file->string pkg-checksum-file))
      (parameterize ([db:current-pkg-catalog-file temp-catalog-file])
        (db:set-pkg! name "local"
                     ""
                     (path->string (path->complete-path pkg-file))
                     new-checksum
                     "")
        (db:set-pkg-dependencies! name "local"
                                  new-checksum
                                  (pkg->deps name))
        (db:set-pkg-modules! name "local"
                             new-checksum
                             (set->list staged-mods)))))

  (define dest-catalog (build-path dest-dir "catalog"))
  (unless quiet?
    (printf/flush "Creating catalog ~a\n" dest-catalog))


  (pkg-catalog-copy (list temp-catalog-file)
                    (build-path dest-dir "catalog")
                    #:force? #t
                    #:override? #t
                    #:relative-sources? relative-sources?)
  (delete-file temp-catalog-file))
