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
         compiler/compilation-path
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
         "dirs.rkt"
         "collects.rkt"
         "addl-installs.rkt")

(provide pkg-install
         pkg-update)

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
  (for ([f (in-directory simple-pkg-path)])
    (define found-pkg (path->pkg f #:cache path-pkg-cache))
    (when (and found-pkg
               (not (equal? found-pkg pkg-name)))
      (pkg-error (~a "cannot link a directory that overlaps with existing packages\n"
                     "  existing package: ~a\n"
                     "  overlapping path: ~a\n"
                     "  a package: ~a")
                 found-pkg
                 f
                 pkg-name)))
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
         #:force-strip? force-strip?
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

    (when (and (pair? orig-pkg)
               (or (eq? (car orig-pkg) 'link)
                   (eq? (car orig-pkg) 'static-link)))
      (disallow-package-path-overlaps pkg-name
                                      pkg-dir
                                      path-pkg-cache
                                      simultaneous-installs))
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
                     #:force-strip? [force-strip? #f]
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
                       #:force-strip? force-strip?
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
       #:force-strip? force-strip?
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
                    #:force-strip? [force-strip? #f]
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
      #:force-strip? force-strip?
      #:all-platforms? all-platforms?
      #:force? force?
      #:ignore-checksums? ignore-checksums?
      #:strict-doc-conflicts? strict-doc-conflicts?
      #:use-cache? use-cache?
      #:link-dirs? link-dirs?
      to-update)]))

;; ----------------------------------------

(define (clear-checksums-in-cache! update-cache)
  (define l (for/list ([(k v) (in-hash update-cache)]
                       #:when (string? v))
              k))
  (for ([k (in-list l)]) (hash-remove! update-cache k)))
