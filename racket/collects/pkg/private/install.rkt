#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/format
         racket/match
         racket/string
         racket/set
         racket/function
         openssl/sha1
         version/utils
         setup/link
         "../path.rkt"
         "../name.rkt"
         "stage.rkt"
         "remove.rkt"
         "desc.rkt"
         "path.rkt"
         "pkg-db.rkt"
         "params.rkt"
         "print.rkt"
         "metadata.rkt"
         "dep.rkt"
         "get-info.rkt"
         "catalog.rkt"
         "dirs.rkt"
         "collects.rkt"
         "addl-installs.rkt"
         "repo-path.rkt"
         "clone-path.rkt"
         "orig-pkg.rkt"
         "info-to-desc.rkt"
         "git.rkt"
         "check-will-exist.rkt"
         "prefetch.rkt")

(provide pkg-install
         pkg-update)

;; A [prefetch-shared] annotation means that a hash table is shared
;; with prefetch threads. If a prefetch group is terminated, then all
;; prefetch-shared tables must be abaondoned, because a thread with a
;; lock can be terminated.

(define (checksum-for-pkg-source pkg-source type pkg-name given-checksum download-printf
                                 #:prefetch? [prefetch? #f]
                                 #:prefetch-group [prefetch-group #f]
                                 #:catalog-lookup-cache [catalog-lookup-cache #f] ; [prefetch-shared]
                                 #:remote-checksum-cache [remote-checksum-cache #f])  ; [prefetch-shared]
  (case type
    [(file-url dir-url github git clone)
     (or given-checksum
         (remote-package-checksum `(url ,pkg-source) download-printf pkg-name
                                  #:type type
                                  #:prefetch? prefetch?
                                  #:prefetch-group prefetch-group
                                  #:catalog-lookup-cache catalog-lookup-cache
                                  #:remote-checksum-cache remote-checksum-cache))]
    [(file)
     (define checksum-pth (format "~a.CHECKSUM" pkg-source))
     (or (and (file-exists? checksum-pth)
	      (file->string checksum-pth))
	 (and (file-exists? pkg-source)
	      (call-with-input-file* pkg-source sha1)))]
    [(name)
     (or given-checksum
         (remote-package-checksum `(catalog ,pkg-source) download-printf pkg-name
                                  #:type type
                                  #:prefetch? prefetch?
                                  #:prefetch-group prefetch-group
                                  #:catalog-lookup-cache catalog-lookup-cache
                                  #:remote-checksum-cache remote-checksum-cache))]
    [else given-checksum]))

(define (disallow-package-path-overlaps pkg-name
                                        pkg-path
                                        path-pkg-cache
                                        simultaneous-installs)
  (define simple-pkg-path (simple-form-path pkg-path))
  (define (one-in-the-other? p1 p2)
    (define pe (explode-path p1))
    (define e (explode-path p2))
    (if ((length e) . < . (length pe))
        (equal? (take pe (length e)) e)
        (equal? (take e (length pe)) pe)))
  ;; Check collects:
  (for ([c (in-list (current-library-collection-paths))])
    (when (one-in-the-other? simple-pkg-path
                             (simple-form-path c))
      (pkg-error (~a "cannot link a directory that overlaps with a collection path\n"
                     "  collection path: ~a\n"
                     "  link path: ~a\n"
                     "  as package: ~a")
                 c
                 pkg-path
                 pkg-name)))
  ;; Check installed packages:
  (when (directory-exists? simple-pkg-path) ; might not exist for a clone shifting to a subdir
    (for ([f (in-directory simple-pkg-path)])
      (define found-pkg (path->pkg f #:cache path-pkg-cache))
      (when (and found-pkg
                 (not (equal? found-pkg pkg-name))
                 ;; In case a new clone dir would overlap with an old one that is being
                 ;; relocated (and if simultaneous installs really overlap, it's caught below):
                 (not (hash-ref simultaneous-installs found-pkg #f)))
        (pkg-error (~a "cannot link a directory that overlaps with existing packages\n"
                       "  existing package: ~a\n"
                       "  overlapping path: ~a\n"
                       "  attempted package: ~a")
                   found-pkg
                   f
                   pkg-name))))
  ;; Check simultaneous installs:
  (for ([(other-pkg other-dir) (in-hash simultaneous-installs)])
    (unless (equal? other-pkg pkg-name)
      (when (one-in-the-other? simple-pkg-path
                               (simple-form-path other-dir))
        (pkg-error (~a "cannot link directories that overlap for different packages\n"
                       "  package: ~a\n"
                       "  path: ~a\n"
                       "  overlapping package: ~a\n"
                       "  overlapping path: ~a")
                   pkg-name
                   pkg-path
                   other-pkg
                   other-dir)))))

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
         #:prefetch-group prefetch-group
         #:catalog-lookup-cache catalog-lookup-cache   ; [prefetch-shared]
         #:remote-checksum-cache remote-checksum-cache ; [prefetch-shared]
         #:updating? updating-all?
         #:extra-updating extra-updating
         #:ignore-checksums? ignore-checksums?
         #:use-cache? use-cache?
         #:skip-installed? skip-installed?
         #:force? force?
         #:all-platforms? all-platforms?
         #:quiet? quiet?
         #:use-trash? use-trash?
         #:from-command-line? from-command-line?
         #:conversation conversation
         #:strip strip-mode
         #:force-strip? force-strip?
         #:link-dirs? link-dirs?
         #:local-docs-ok? local-docs-ok?
         #:ai-cache ai-cache
         #:clone-info clone-info
         #:pull-behavior pull-behavior
         #:dry-run? dry-run?
         descs)
  (define download-printf (if quiet? void printf/flush))
  (define check-sums? (not ignore-checksums?))
  (define current-scope-db (read-pkg-db))
  (define all-db (merge-pkg-dbs))
  (define path-pkg-cache (make-hash))
  (define (install-package/outer infos desc info)
    (match-define (pkg-desc pkg type orig-name given-checksum auto? pkg-extra-path) desc)
    (match-define
     (install-info pkg-name orig-pkg pkg-dir git-dir clean? checksum module-paths additional-installs)
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

    (when (and (pair? orig-pkg)
               (or (eq? (car orig-pkg) 'link)
                   (eq? (car orig-pkg) 'static-link)
                   (eq? (car orig-pkg) 'clone)))
      (disallow-package-path-overlaps pkg-name
                                      (if (eq? (car orig-pkg) 'clone)
                                          git-dir
                                          pkg-dir)
                                      path-pkg-cache
                                      simultaneous-installs))
    
    (define updating? (or updating-all?
                          (hash-ref extra-updating pkg-name #f)))
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
              (same-orig-pkg? (pkg-info-orig-pkg existing-pkg-info) orig-pkg)
              ;; Also, make sure it's installed in the scope that we're changing:
              (hash-ref current-scope-db pkg-name #f))
         ;; promote an auto-installed package to a normally installed one
         (cons
          #f ; no repo change
          ;; The `do-it` thunk:
          (lambda (fail-repos)
            (unless quiet?
              (download-printf "Promoting ~a from auto-installed to explicitly installed~a\n"
                               pkg-name
                               (dry-run-explain dry-run?)))
            (unless dry-run?
              (update-pkg-db! pkg-name (update-auto existing-pkg-info #f)))))]
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
          (define found-f
            (and f
                 ;; Check for source or compiled; may need to use a slower process to
                 ;; find the relevant one:
                 (check-found-module-will-exist f mp metadata-ns)))
          (cond
           [(and found-f
                 ;; If it's from a simultaneous install, we'll want to check the updated
                 ;; version of the package, instead:
                 (not (hash-ref simultaneous-installs (path->pkg found-f #:cache path-pkg-cache) #f)))
            ;; This module is already installed
            (cons (path->pkg found-f #:cache path-pkg-cache) mp)]
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
             (pkg-error (~a "package conflicts with existing installed module;\n"
                            " the existing installed module is not part of a package\n"
                            "  package: ~a\n"
                            "  module path: ~s\n"
                            "  potentially relevant paths:~a")
                        pkg
                        (pretty-module-path mp)
                        (format-list
                         (for/list ([p (in-list (append
                                                 (current-library-collection-paths)
                                                 (current-library-collection-links)))]
                                    #:when (path? p))
                           p)))))]
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
            (raise (vector updating? infos pkg-name unsatisfied-deps void 'always-yes clone-info))]
           ['search-ask
            (show-dependencies unsatisfied-deps #f #f)
            (case (if (eq? conversation 'always-yes)
                      'always-yes
                      (ask "Would you like to install these dependencies?"))
              [(yes)
               (raise (vector updating? infos pkg-name unsatisfied-deps void 'again clone-info))]
              [(always-yes)
               (raise (vector updating? infos pkg-name unsatisfied-deps void 'always-yes clone-info))]
              [(cancel)
               (clean!)
               (pkg-error "canceled")]
              [(no)
               (clean!)
               (pkg-error "missing dependencies\n  missing packages:~a" (format-list unsatisfied-deps))])]))]
      [(and
        (or do-update-deps?
            update-implies?)
        (let ()
          (define-values (run-deps build-deps) (get-all-deps* metadata-ns pkg-dir))
          (define deps (append run-deps build-deps))
          (define implies (list->set
                           (append
                            (get-all-implies metadata-ns pkg-dir run-deps)
                            (get-all-update-implies metadata-ns pkg-dir deps))))
          (define update-pkgs
            (append-map (λ (dep)
                           (define name (dependency->name dep))
                           (define this-platform? (or all-platforms?
                                                      (dependency-this-platform? dep)))
                           (or (and this-platform?
                                    (or do-update-deps?
                                        (set-member? implies name))
                                    (not (hash-ref simultaneous-installs name #f))
                                    (let ([updater
                                           (packages-to-update download-printf current-scope-db
                                                               #:all-db all-db
                                                               #:must-update? #f
                                                               #:deps? do-update-deps?
                                                               #:implies? update-implies?
                                                               #:update-cache update-cache
                                                               #:prefetch-group prefetch-group
                                                               #:namespace metadata-ns
                                                               #:catalog-lookup-cache catalog-lookup-cache
                                                               #:remote-checksum-cache remote-checksum-cache
                                                               #:all-platforms? all-platforms?
                                                               #:ignore-checksums? ignore-checksums?
                                                               #:use-cache? use-cache?
                                                               #:from-command-line? from-command-line?
                                                               #:link-dirs? link-dirs?)])
                                      (updater #:prefetch? #t name)
                                      (updater name)))
                               null))
                        deps))
          (and (not (empty? update-pkgs))
               update-pkgs
               (let ()
                 (define (continue conversation)
                   (raise (vector #t infos pkg-name update-pkgs
                                  (λ () (for-each (compose (remove-package #t quiet? use-trash? dry-run?) pkg-desc-name) update-pkgs))
                                  conversation
                                  clone-info)))
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
                      [(cancel)
                       (clean!)
                       (pkg-error "canceled")]
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
              (let ([to-update (let ([updater (packages-to-update download-printf current-scope-db
                                                                  #:all-db all-db
                                                                  #:deps? update-deps? 
                                                                  #:implies? update-implies?
                                                                  #:update-cache update-cache
                                                                  #:prefetch-group prefetch-group
                                                                  #:namespace metadata-ns
                                                                  #:catalog-lookup-cache catalog-lookup-cache
                                                                  #:remote-checksum-cache remote-checksum-cache
                                                                  #:all-platforms? all-platforms?
                                                                  #:ignore-checksums? ignore-checksums?
                                                                  #:use-cache? use-cache?
                                                                  #:from-command-line? from-command-line?
                                                                  #:link-dirs? link-dirs?)])
                                 (for ([pkg (in-list update-pkgs)]) (updater #:prefetch? #t pkg))
                                 (append-map updater update-pkgs))])
                (λ () (for-each (compose (remove-package #t quiet? use-trash? dry-run?) pkg-desc-name) to-update))))
            (match this-dep-behavior
              ['fail
               (clean!)
               (report-mismatch update-deps)]
              ['search-auto
               (show-dependencies update-deps #t #t)
               (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'always-yes clone-info))]
              ['search-ask
               (show-dependencies update-deps #t #f)
               (case (if (eq? conversation 'always-yes)
                         'always-yes
                         (ask "Would you like to update these dependencies?"))
                 [(yes)
                  (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'again clone-info))]
                 [(always-yes)
                  (raise (vector #t infos pkg-name update-pkgs (make-pre-succeed) 'always-yes clone-info))]
                 [(cancel)
                  (clean!)
                  (pkg-error "canceled")]
                 [(no)
                  (clean!)
                  (report-mismatch update-deps)])]))]
      [else
       (define repo (and git-dir
                         (enclosing-path-for-repo (caddr orig-pkg) git-dir)))
       (cons
        ;; The repo to get new commits, if any:
        (and repo (list repo
                        checksum))
        ;; The "do-it" function (see `repos+do-its` below):
        (λ (fail-repos)
          (when updating?
            (download-printf "Re-installing ~a~a\n" pkg-name (dry-run-explain dry-run?)))
          (define final-pkg-dir
            (cond
             [clean?
              (define final-pkg-dir (or git-dir
                                        (select-package-directory
                                         (build-path (pkg-installed-dir) pkg-name)
                                         dry-run?)))
              (unless dry-run?
                (unless git-dir
                  (make-parent-directory* final-pkg-dir)
                  (copy-directory/files pkg-dir final-pkg-dir #:keep-modify-seconds? #t))
                (clean!))
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
          (unless dry-run?
            (links final-pkg-dir
                   #:name single-collect
                   #:user? (not (or (eq? 'installation scope)
                                    (path? scope)))
                   #:file (scope->links-file scope)
                   #:root? (not single-collect)
                   #:static-root? (and (pair? orig-pkg)
                                       (eq? 'static-link (car orig-pkg)))))
          (define alt-dir-name
            ;; If we had to pick an alternate dir name, then record it:
            (let-values ([(base name dir?) (split-path final-pkg-dir)])
              (and (path? name)
                   (regexp-match? #rx"[+]" name)
                   (path->string name))))
          (define new-checksum
            (if (hash-ref fail-repos repo #f)
                ;; Failed `git pull` => record checksum as #f, because we've lost track
                ;; of the state of this package:
                #f
                checksum))
          (define this-pkg-info
            (make-pkg-info orig-pkg new-checksum auto? single-collect alt-dir-name))
          (log-pkg-debug "updating db with ~e to ~e" pkg-name this-pkg-info)
          (unless dry-run?
            (update-pkg-db! pkg-name this-pkg-info))))]))
  (define metadata-ns (make-metadata-namespace))
  (define infos
    (for/list ([v (in-list descs)])
      (stage-package/info (pkg-desc-source v) (pkg-desc-type v) (pkg-desc-name v)
                          #:at-dir (pkg-desc-extra-path v)
                          #:given-checksum (pkg-desc-checksum v)
                          #:use-cache? use-cache?
                          check-sums? download-printf
                          metadata-ns
                          #:catalog-lookup-cache catalog-lookup-cache
                          #:remote-checksum-cache remote-checksum-cache
                          #:strip strip-mode
                          #:force-strip? force-strip?
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

  (define repo+do-its ; list of (cons #f-or-(list git-dir checksum) do-it-thunk)
    (map (curry install-package/outer all-infos)
         all-descs
         all-infos))
  
  ;; collapse planned repo actions:
  (define repos
    (for/fold ([ht (hash)]) ([repo+do-it (in-list repo+do-its)])
      (define repo (car repo+do-it))
      (cond
       [repo
        (define git-dir (car repo))
        (define checksum (cadr repo))
        (define prev-checksums (hash-ref ht git-dir null))
        (if (member checksum prev-checksums)
            ht
            (hash-set ht git-dir (cons checksum prev-checksums)))]
       [else ht])))

  ;; relevant commits have been fecthed to the repos, and now we need
  ;; to check them out; if a checkout fails, then we've left the
  ;; package installation in no worse shape than if a manual `git
  ;; pull` failed
  (define fail-repos
    (for/fold ([fail-repos #hash()]) ([(git-dir checksums) (in-hash repos)])
      (parameterize ([current-directory git-dir])
        (download-printf "Merging commits at ~a~a\n"
                         git-dir
                         (dry-run-explain dry-run?))
        (when ((length checksums) . > . 1)
          (download-printf (~a "Multiple packages in the of the clone\n"
                               "  " git-dir "\n"
                               " have different target commits; will try each commit, which will work\n"
                               " as long as some commit is a fast-forward of all of them\n")))
        (for/fold ([fail-repos fail-repos]) ([checksum (in-list checksums)])
          (define rebase? (eq? pull-behavior 'rebase))
          (define ok?
            (git #:status (lambda (s) (download-printf "~a\n" s))
                 #:fail-mode 'status
                 #:dry-run? dry-run?
                 (if rebase? "rebase" "merge")
                 (if rebase? "--onto" "--ff-only")
                 checksum))
          (cond
           [ok? fail-repos]
           [else
            (case pull-behavior
              [(try)
               (download-printf (~a "Pulling commits failed, but continuing anyway~a\n")
                                (if from-command-line?
                                    " due to `--pull try'"
                                    ""))
               (hash-set fail-repos git-dir #t)]
              [else
               (pkg-error (~a "pulling commits to clone failed~a\n"
                              "  clone: ~a\n"
                              "  target commit: ~a")
                          (if from-command-line?
                              ";\n fix clone manually or use `--pull try' or `--pull rebase'"
                              "")
                          git-dir
                          checksum)])])))))

  ;; pre-succeed removes packages that are being updated
  (pre-succeed)

  (define post-metadata-ns (make-metadata-namespace))
  ;; moves packages into place and installs links:
  (for-each (λ (t) ((cdr t) fail-repos)) repo+do-its)

  (define (is-promote? info)
    ;; if the package name is in `current-scope-db', we must
    ;; be simply promiting the package, and so it's
    ;; already set up:
    (and (hash-ref current-scope-db (install-info-name info) #f) #t))

  
  (define updating-any?
    (or updating-all? (positive? (hash-count extra-updating))))
  
  (define setup-collects
    (let ([db (read-pkg-db)])
      (get-setup-collects ((if updating-any?
                               (make-close-over-depending (read-pkg-db)
                                                          post-metadata-ns
                                                          all-platforms?)
                               values)
                           (map install-info-name
                                (if updating-any?
                                    all-infos
                                    (filter-not is-promote? all-infos))))
                          db
                          post-metadata-ns)))

  (cond
   [(or (null? repo+do-its)
        (and (not updating-any?) (andmap is-promote? all-infos))
        dry-run?)
    ;; No actions, so no setup:
    'skip]
   [else
    setup-collects]))

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

(define (select-package-directory dir dry-run? #:counter [counter 0])
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
   [(and (directory-exists? full-dir)
         (not dry-run?))
    ;; If the directory exists, assume that we'd like to replace it.
    ;; Maybe the directory couldn't be deleted when a package was
    ;; uninstalled, and maybe it will work now (because some process
    ;; has completed on Windows or some other filesystem with locks).
    (with-handlers ([exn:fail:filesystem?
                     (lambda (exn)
                       (log-pkg-warning "error deleting old directory: ~a" 
                                        (exn-message exn))
                       (select-package-directory dir #f #:counter (add1 counter)))])
      (delete-directory/files full-dir)
      ;; delete succeeded:
      full-dir)]
   [else
    ;; all clear to use the selected name:
    full-dir]))

(define (snoc l x)
  (append l (list x)))

(define (pkg-install given-descs
                     #:old-infos [old-infos empty]
                     #:old-descs [old-descs empty]
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
                     #:prefetch-group [prefetch-group (make-prefetch-group)]
                     #:catalog-lookup-cache [catalog-lookup-cache (make-hash)]   ; [prefetch-shared]
                     #:remote-checksum-cache [remote-checksum-cache (make-hash)] ; [prefetch-shared]
                     #:check-pkg-early? [check-pkg-early? #t]
                     #:updating? [updating? #f]
                     #:quiet? [quiet? #f]
                     #:use-trash? [use-trash? #f]
                     #:from-command-line? [from-command-line? #f]
                     #:conversation [conversation #f]
                     #:strip [strip-mode #f]
                     #:force-strip? [force-strip? #f]
                     #:link-dirs? [link-dirs? #f]
                     #:summary-deps [summary-deps empty]
                     #:multi-clone-behavior [old-clone-behavior 'fail]
                     #:repo-descs [old-repo-descs (initial-repo-descs
                                                   (read-pkg-db)
                                                   (if quiet? void printf/flush))]
                     #:pull-behavior [pull-behavior 'ff-only]
                     #:convert-to-non-clone? [convert-to-non-clone? #f]
                     #:dry-run? [dry-run? #f])
  (define download-printf (if quiet? void printf/flush))
  
  (define descs
    (map (convert-clone-name-to-clone-repo/install catalog-lookup-cache
                                                   download-printf)
         given-descs))

  (define all-scope-dbs (and skip-installed? (merge-pkg-dbs)))
  (define db (and check-pkg-early? (read-pkg-db)))

  (define filtered-descs
    (remove-duplicates
     (if (not skip-installed?)
         descs
         (filter (lambda (d)
                   (define pkg-name (desc->name d))
                   (define i (hash-ref all-scope-dbs pkg-name #f))
                   (or (not i) (pkg-info-auto? i)))
                 descs))
     pkg-desc=?))

  (unless (or updating?
              skip-installed?
              force)
    (early-check-for-installed filtered-descs db #:wanted? #f))

  (call-with-prefetch-cleanup
   prefetch-group
   (lambda ()
     (define-values (new-descs done-descs done-infos clone-behavior repo-descs
                               extra-updating)
       (adjust-to-normalize-repos filtered-descs old-descs old-infos
                                  old-clone-behavior old-repo-descs
                                  updating?
                                  catalog-lookup-cache
                                  download-printf
                                  from-command-line?
                                  convert-to-non-clone?
                                  prefetch-group))

    (with-handlers* ([vector?
                      (match-lambda
                       [(vector updating? new-infos dep-pkg deps more-pre-succeed conv clone-info)
                        (pkg-install
                         #:summary-deps (snoc summary-deps (vector dep-pkg deps))
                         #:old-infos new-infos
                         #:old-descs (append done-descs new-descs)
                         #:all-platforms? all-platforms?
                         #:force? force
                         #:ignore-checksums? ignore-checksums?
                         #:strict-doc-conflicts? strict-doc-conflicts?
                         #:use-cache? use-cache?
                         #:dep-behavior dep-behavior
                         #:update-deps? update-deps?
                         #:update-implies? update-implies?
                         #:update-cache update-cache
                         #:prefetch-group prefetch-group
                         #:catalog-lookup-cache catalog-lookup-cache
                         #:remote-checksum-cache remote-checksum-cache
                         #:check-pkg-early? #f
                         #:pre-succeed (lambda () (pre-succeed) (more-pre-succeed))
                         #:updating? updating?
                         #:quiet? quiet?
                         #:use-trash? use-trash?
                         #:from-command-line? from-command-line?
                         #:conversation conv
                         #:strip strip-mode
                         #:force-strip? force-strip?
                         #:multi-clone-behavior (vector-ref clone-info 0)
                         #:repo-descs (vector-ref clone-info 1)
                         #:pull-behavior pull-behavior
                         #:dry-run? dry-run?
                         (for/list ([dep (in-list deps)])
                           (if (pkg-desc? dep)
                               dep
                               (pkg-desc dep #f #f #f #t #f))))])])
      (begin0
       (install-packages
        #:old-infos done-infos
        #:old-descs done-descs
        #:all-platforms? all-platforms?
        #:force? force
        #:ignore-checksums? ignore-checksums?
        #:use-cache? use-cache?
        #:skip-installed? skip-installed?
        #:dep-behavior dep-behavior
        #:update-deps? update-deps?
        #:update-implies? update-implies?
        #:update-cache update-cache
        #:prefetch-group prefetch-group
        #:catalog-lookup-cache catalog-lookup-cache
        #:remote-checksum-cache remote-checksum-cache
        #:pre-succeed (λ ()
                        (for ([pkg-name (in-hash-keys extra-updating)])
                          ((remove-package #t quiet? use-trash? dry-run?) pkg-name))
                        (pre-succeed))
        #:updating? updating?
        #:extra-updating extra-updating
        #:quiet? quiet?
        #:use-trash? use-trash?
        #:from-command-line? from-command-line?
        #:conversation conversation
        #:strip strip-mode
        #:force-strip? force-strip?
        #:link-dirs? link-dirs?
        #:local-docs-ok? (not strict-doc-conflicts?)
        #:ai-cache (box #f)
        #:clone-info (vector clone-behavior
                             repo-descs)
        #:pull-behavior pull-behavior
        #:dry-run? dry-run?
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
                                        (format-list ds)))))))))))))

;; Determine packages to update, starting with `pkg-name'. If `pkg-name'
;; needs to be updated, return it in a list. Otherwise, if `deps?',
;; then return a list of dependencies that need to be updated.
;; (If a package needs to be updated, wait until the update
;; has been inspected for further dependencies.)
;; If `must-update?', then complain if the package is not
;; updatable.
;; The `update-cache' argument is used to cache which packages
;; are already being updated and their downloaded checksums;
;; it maps a package name to a checksum, and a box of the package
;; name to #t (to avoid multiple update attempts).
(define ((packages-to-update download-printf db
                             #:all-db all-db
                             #:must-update? [must-update? #t]
                             #:deps? deps?
                             #:implies? implies?
                             #:namespace metadata-ns 
                             #:catalog-lookup-cache catalog-lookup-cache   ; [prefetch-shared]
                             #:remote-checksum-cache remote-checksum-cache ; [prefetch-shared]
                             #:update-cache update-cache
                             #:prefetch-group prefetch-group
                             #:all-platforms? all-platforms?
                             #:ignore-checksums? ignore-checksums?
                             #:use-cache? use-cache?
                             #:from-command-line? from-command-line?
                             #:link-dirs? link-dirs?
                             #:skip-uninstalled? [skip-uninstalled? #f]
                             #:all-mode? [all-mode? #f]
                             #:force-update? [force-update? #f])
         pkg-name
         ;; In prefetch mode, do as much work as possible to generate
         ;; server requests without waiting for results and without
         ;; making any other state changes --- but forced errors are
         ;; ok.
         #:prefetch? [prefetch? #f])
  (let update-loop ([pkg-name pkg-name]
                    [must-update? must-update?]
                    [force-update? force-update?]
                    [report-skip? #t]
                    [prefetch? prefetch?])
    (cond
     [(pkg-desc? pkg-name)
      ;; Infer the package-source type and name:
      (define-values (inferred-name type) (package-source->name+type
                                           (pkg-desc-source pkg-name)
                                           (pkg-desc-type pkg-name)
                                           #:link-dirs? link-dirs?
                                           #:must-infer-name? (not (pkg-desc-name pkg-name))
                                           #:complain (complain-about-source (pkg-desc-name pkg-name))))
      (define name (or (pkg-desc-name pkg-name)
                       inferred-name))
      ;; Check that the package is installed, and get current checksum:
      (define info (package-info name #:db db (not skip-uninstalled?)))
      (cond
       [(not info)
        ;; Not installed, and we're skipping uninstalled
        null]
       [else
        (define new-checksum (checksum-for-pkg-source (pkg-desc-source pkg-name)
                                                      type
                                                      name
                                                      (pkg-desc-checksum pkg-name)
                                                      download-printf
                                                      #:prefetch? prefetch?
                                                      #:prefetch-group prefetch-group
                                                      #:catalog-lookup-cache catalog-lookup-cache
                                                      #:remote-checksum-cache remote-checksum-cache))
        (cond
         [prefetch?
          ;; Don't proceed further if we're just issuing prefetches
          null]
         [else
          (hash-set! update-cache name new-checksum) ; record downloaded checksum
          (unless (or ignore-checksums? (not (pkg-desc-checksum pkg-name)))
            (unless (equal? (pkg-desc-checksum pkg-name) new-checksum)
              (pkg-error (~a "incorrect checksum on package\n"
                             "  package source: ~a\n"
                             "  expected: ~e\n"
                             "  got: ~e")
                         (pkg-desc-source pkg-name)
                         (pkg-desc-checksum pkg-name) 
                         new-checksum)))
          
          (if (or force-update?
                  ;; Different checksum => update
                  (not (equal? (pkg-info-checksum info)
                               new-checksum))
                  ;; No checksum available => always update
                  (not new-checksum)
                  ;; Different source => always update
                  (not (same-orig-pkg? (pkg-info-orig-pkg info)
                                       (desc->orig-pkg type
                                                       (pkg-desc-source pkg-name)
                                                       (pkg-desc-extra-path pkg-name)))))
              ;; Update:
              (begin
                (hash-set! update-cache (box name) #t)
                (list (pkg-desc (pkg-desc-source pkg-name)
                                (pkg-desc-type pkg-name)
                                name
                                (pkg-desc-checksum pkg-name)
                                (pkg-desc-auto? pkg-name)
                                (or (pkg-desc-extra-path pkg-name)
                                    (and (eq? type 'clone)
                                         (current-directory))))))
              ;; No update needed, but maybe check dependencies:
              (if (or deps?
                      implies?)
                  (update-loop name #f #f #f prefetch?)
                  null))])])]
     [(and prefetch?
           (hash-ref (prefetch-group-in-progress prefetch-group) pkg-name #f))
      ;; Already covered for prefetch
      null]
     [(and (not prefetch?)
           (hash-ref update-cache (box pkg-name) #f))
      ;; package is already being updated
      null]
     ;; A string indicates that package source that should be
     ;; looked up in the installed packages to get the old source
     ;; for getting the checksum:
     [(package-info pkg-name #:db db (and must-update?
                                          (not skip-uninstalled?)))
      =>
      (lambda (info)
        (match-define (pkg-info orig-pkg checksum auto?) info)
        
        (define deps
          (if (or deps? implies?)
              ((package-dependencies metadata-ns db all-platforms? 
                                     #:only-implies? (not deps?))
               pkg-name)
              null))
        
        (define (check-missing-dependencies k)
          (define missing-deps
            (for/list ([dep (in-list deps)]
                       #:unless (equal? dep "racket")
                       #:unless (package-info dep #:db all-db #f))
              dep))
          (cond
           [(pair? missing-deps)
            ;; A dependency is missing. Treat the dependent package as
            ;; needing an update, even if it is installed as a link, so
            ;; that the user is asked about installing dependencies, etc.
            (log-pkg-debug "Missing dependencies of ~s: ~s" pkg-name missing-deps)
            (update-loop (pkg-info->desc pkg-name info) #f #t #t prefetch?)]
           [else (k)]))
          
        (define (update-dependencies)
          ;; Mark in progress:
          (if prefetch?
              (hash-set! (prefetch-group-in-progress prefetch-group) pkg-name #t)
              (hash-set! update-cache (box pkg-name) #t))
          ;; Dependencies?
          (if (or deps? implies?)
              ;; Check dependencies
              (append-map
               (lambda (dep) (update-loop dep #f #f #t prefetch?))
               deps)
              null))
        
        (define (skip/update-dependencies kind)
          (check-missing-dependencies
           (lambda ()
             (unless (or all-mode? (not report-skip?) prefetch?)
               (download-printf "Skipping update of ~a: ~a\n"
                                kind
                                pkg-name))
             (update-dependencies))))
        
        (match orig-pkg
          [`(,(or 'link 'static-link) ,orig-pkg-dir)
           (if must-update?
               (pkg-error (~a "cannot update linked packages;\n"
                              " except with a replacement package source\n"
                              "  package name: ~a\n"
                              "  package source: ~a")
                          pkg-name
                          (simple-form-path
                           (path->complete-path orig-pkg-dir (pkg-installed-dir))))
               (skip/update-dependencies "linked package"))]
          [`(dir ,_)
           (if must-update?
               (pkg-error (~a "cannot update packages installed locally;\n"
                              " except with a replacement package source;\n"
                              " package was installed via a local directory\n"
                              "  package name: ~a")
                          pkg-name)
               (skip/update-dependencies "package installed locally"))]
          [`(file ,_)
           (if must-update?
               (pkg-error (~a "cannot update packages installed locally;\n"
                              " except with a replacement package source;\n"
                              " package was installed via a local file\n"
                              "  package name: ~a")
                          pkg-name)
               (skip/update-dependencies "package installed locally"))]
          [_
           (define-values (orig-pkg-source orig-pkg-type orig-pkg-dir)
             (if (eq? 'clone (car orig-pkg))
                 (values (caddr orig-pkg)
                         'clone
                         (enclosing-path-for-repo (caddr orig-pkg)
                                                  (path->complete-path
                                                   (cadr orig-pkg)
                                                   (pkg-installed-dir))))
                 ;; It would be better if the type were preseved
                 ;; from install time, but we always make the
                 ;; URL unambigious:
                 (values (cadr orig-pkg) #f #f)))
           (define new-checksum
             (hash-ref update-cache pkg-name
                       (lambda ()
                         (remote-package-checksum orig-pkg download-printf pkg-name
                                                  #:prefetch? prefetch?
                                                  #:prefetch-group prefetch-group
                                                  #:catalog-lookup-cache catalog-lookup-cache
                                                  #:remote-checksum-cache remote-checksum-cache))))
           ;; Record downloaded checksum:
           (unless prefetch?
             (hash-set! update-cache pkg-name new-checksum))
           (or (and new-checksum
                    (not (equal? checksum new-checksum))
                    ;; Update it:
                    (cond
                     [prefetch?
                      ;; Don't proceed further if we're just issuing prefetches
                      null]
                     [else
                      ;; Flush cache of downloaded checksums, in case
                      ;; there was a race between our checkig and updates on
                      ;; the catalog server:
                      (clear-checksums-in-cache! update-cache)
                      (list (pkg-desc orig-pkg-source orig-pkg-type pkg-name #f auto?
                                      orig-pkg-dir))]))
               ;; Continue with dependencies, maybe
               (check-missing-dependencies update-dependencies))]))]
     [else null])))

(define (pkg-update in-pkgs
                    #:all? [all? #f]
                    #:dep-behavior [dep-behavior #f]
                    #:all-platforms? [all-platforms? #f]
                    #:force? [force? #f]
                    #:ignore-checksums? [ignore-checksums? #f]
                    #:strict-doc-conflicts? [strict-doc-conflicts? #f]
                    #:skip-uninstalled? [skip-uninstalled? #f]
                    #:use-cache? [use-cache? #t]
                    #:update-deps? [update-deps? #f]
                    #:update-implies? [update-implies? #t]
                    #:quiet? [quiet? #f]
                    #:use-trash? [use-trash? #f]
                    #:from-command-line? [from-command-line? #f]
                    #:strip [strip-mode #f]
                    #:force-strip? [force-strip? #f]
                    #:link-dirs? [link-dirs? #f]
                    #:infer-clone-from-dir? [infer-clone-from-dir? #f]
                    #:lookup-for-clone? [lookup-for-clone? #f]
                    #:multi-clone-behavior [clone-behavior 'fail]
                    #:pull-behavior [pull-behavior 'ff-only]
                    #:dry-run? [dry-run? #f])
  (define download-printf (if quiet? void printf/flush))
  (define metadata-ns (make-metadata-namespace))
  (define db (read-pkg-db))
  (define all-db (merge-pkg-dbs))
  (define all-mode? (and all? (empty? in-pkgs)))
  (define pkgs (cond
                [all-mode? (hash-keys db)]
                [else
                 (unless (or skip-uninstalled?
                             force?)
                   (early-check-for-installed in-pkgs db #:wanted? #t))
                 in-pkgs]))
  (define update-cache (make-hash))
  (define prefetch-group (make-prefetch-group))
  (define catalog-lookup-cache (make-hash))  ; [prefetch-shared]
  (define remote-checksum-cache (make-hash)) ; [prefetch-shared]
  (call-with-prefetch-cleanup
   prefetch-group
   (lambda ()
     (define to-updat* (let ([updater (packages-to-update download-printf db
                                                          #:all-db all-db
                                                          #:must-update? (and (not all-mode?)
                                                                              (not update-deps?))
                                                          #:deps? (or update-deps? 
                                                                      all-mode?) ; avoid races
                                                          #:implies? update-implies?
                                                          #:update-cache update-cache
                                                          #:prefetch-group prefetch-group
                                                          #:namespace metadata-ns
                                                          #:catalog-lookup-cache catalog-lookup-cache
                                                          #:remote-checksum-cache remote-checksum-cache
                                                          #:all-platforms? all-platforms?
                                                          #:ignore-checksums? ignore-checksums?
                                                          #:use-cache? use-cache?
                                                          #:from-command-line? from-command-line?
                                                          #:skip-uninstalled? skip-uninstalled?
                                                          #:link-dirs? link-dirs?
                                                          #:all-mode? all-mode?)]
                             [pkgs (map (compose
                                         (if infer-clone-from-dir?
                                             (convert-directory-to-installed-clone db)
                                             values)
                                         (if lookup-for-clone?
                                             (convert-clone-name-to-clone-repo/install catalog-lookup-cache
                                                                                       download-printf)
                                             (convert-clone-name-to-clone-repo/update db
                                                                                      skip-uninstalled?
                                                                                      from-command-line?)))
                                        pkgs)])
                         ;; Prefetch packages info and checksums:
                         (for ([pkg (in-list pkgs)]) (updater #:prefetch? #t pkg))
                         ;; Build update info:
                         (append-map updater pkgs)))
     (cond
      [(empty? pkgs)
       (unless quiet?
         (cond
          [all?
           (printf/flush (~a "No updates available; no packages installed in ~a scope\n")
                         (current-pkg-scope))]
          [else
           (printf/flush (~a "No packages given to update"
                             (if from-command-line?
                                 (~a
                                  ";\n use `--all' to update all packages, or run from a package's directory"
                                  "\n to update that package")
                                 "")
                             "\n"))]))
       'skip]
      [(empty? to-updat*)
       (unless quiet?
         (printf/flush "No updates available\n"))
       'skip]
      [else
       (define to-update
         (hash-values
          (for/fold ([ht #hash()]) ([u (in-list to-updat*)])
            (cond
             [(hash-ref ht (pkg-desc-name u) #f)
              => (lambda (v)
                   (cond
                    [(pkg-desc=? v u) ht]
                    [else
                     (pkg-error (~a "cannot update with conflicting update information;\n"
                                    "  package name: ~a")
                                (pkg-desc-name u))]))]
             [else
              (hash-set ht (pkg-desc-name u) u)]))))
       (unless quiet?
         (printf "Updating:\n")
         (for ([u (in-list to-update)])
           (printf "  ~a\n" (pkg-desc-name u)))
         (flush-output))
       (pkg-install
        #:updating? #t
        #:pre-succeed (λ () (for-each (compose (remove-package #t quiet? use-trash? dry-run?) pkg-desc-name) to-update))
        #:dep-behavior dep-behavior
        #:update-deps? update-deps?
        #:update-implies? update-implies?
        #:update-cache update-cache
        #:prefetch-group prefetch-group
        #:catalog-lookup-cache catalog-lookup-cache
        #:remote-checksum-cache remote-checksum-cache
        #:check-pkg-early? #f
        #:quiet? quiet?
        #:use-trash? use-trash?
        #:from-command-line? from-command-line?
        #:strip strip-mode
        #:force-strip? force-strip?
        #:all-platforms? all-platforms?
        #:force? force?
        #:ignore-checksums? ignore-checksums?
        #:strict-doc-conflicts? strict-doc-conflicts?
        #:use-cache? use-cache?
        #:link-dirs? link-dirs?
        #:multi-clone-behavior clone-behavior
        #:convert-to-non-clone? (and lookup-for-clone?
                                     (andmap pkg-desc? in-pkgs)
                                     (not (ormap pkg-desc-extra-path in-pkgs)))
        #:pull-behavior pull-behavior
        #:dry-run? dry-run?
        to-update)]))))

;; ----------------------------------------

(define (early-check-for-installed in-pkgs db #:wanted? wanted?)
  (for ([d (in-list in-pkgs)])
    (define-values (name ignored-type)
      (if (pkg-desc? d)
          ;; For install of update:
          (cond
           [(pkg-desc-name d)
            (values (pkg-desc-name d) #f)]
           [(and (eq? (pkg-desc-type d) 'clone)
                 ;; If syntax of the source is a package name, then it's a package name:
                 (let-values ([(name type) (package-source->name+type (pkg-desc-source d) 'name)])
                   name))
            => (lambda (name)
                 (values name #f))]
           [else
            (package-source->name+type (pkg-desc-source d)
                                       (pkg-desc-type d)
                                       #:must-infer-name? #t
                                       #:complain (complain-about-source #f))])
          ;; Must be a string package name for update:
          (values d #f)))
    (define info (package-info name wanted? #:db db))
    (when (and info
               (not wanted?)
               (not (pkg-info-auto? info)))
      (pkg-error (~a "package is already installed\n"
                     "  package: ~a")
                 name))))

;; ----------------------------------------

(define (clear-checksums-in-cache! update-cache)
  (define l (for/list ([(k v) (in-hash update-cache)]
                       #:when (string? v))
              k))
  (for ([k (in-list l)]) (hash-remove! update-cache k)))
