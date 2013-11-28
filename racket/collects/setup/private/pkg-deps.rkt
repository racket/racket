#lang racket/base
(require syntax/modread
         syntax/modcollapse
         pkg/lib
         pkg/name
         racket/set
         racket/string
         racket/list
         setup/getinfo
         racket/file
         racket/path
         setup/dirs
         setup/doc-db)

(provide check-package-dependencies)

(define core-pkg "base")

;; Submodules with these names are dropped in binary
;; packages, so they only controbute to `build-deps':
(define build-only-submod-names '(test doc srcdoc))

(define (check-package-dependencies
         paths
         coll-paths
         coll-main?s
         coll-modes
         setup-printf setup-fprintf
         check-unused? fix? verbose?)
  ;; Tables
  (define missing (make-hash))
  (define skip-pkgs (make-hash))
  (define pkg-internal-deps (make-hash)) ; dependencies available for a package's own use
  (define pkg-immediate-deps (make-hash)) ; save immediate dependencies
  (define pkg-external-deps (make-hash)) ; dependencies made available though `implies'
  (define pkg-actual-deps (make-hash)) ; found dependencies (when checking for unused)
  (define pkg-implies (make-hash)) ; for checking unused
  (define pkg-reps (make-hash)) ; for union-find on external deps
  (define mod-pkg (make-hash))
  (define path-cache (make-hash))
  (define metadata-ns (make-base-namespace))

  ;; ----------------------------------------
  ;; printinf helpers:
  (define (setup-printf* task s . args)
    (for ([s (string-split (apply format s args) "\n")])
      (setup-printf task s)))
  (define (setup-fprintf* o task s . args)
    (for ([s (string-split (apply format s args) "\n")])
      (setup-fprintf o task s)))
  
  ;; ----------------------------------------
  ;; Find the canonical representative for a set of external dependencies:
  (define (find-rep! pkg)
    (define rep-pkg (hash-ref pkg-reps pkg))
    (if (equal? rep-pkg pkg)
        pkg
        (let ([rep-pkg (find-rep! rep-pkg)])
          (hash-set! pkg-reps pkg rep-pkg)
          rep-pkg)))
  
  ;; ----------------------------------------
  ;; Equate `a-pkg' and `b-pkg', returning a representative:
  (define (union-find! a-pkg b-pkg)
    (define rep-a-pkg (find-rep! a-pkg))
    (define rep-b-pkg (find-rep! b-pkg))
    (unless (equal? rep-a-pkg rep-b-pkg)
      (define a-deps (hash-ref pkg-reps rep-a-pkg))
      (define b-deps (hash-ref pkg-reps rep-b-pkg))
      (hash-set! pkg-reps rep-b-pkg (set-union a-deps b-deps))
      (hash-remove! pkg-external-deps rep-a-pkg)
      (hash-set! pkg-reps rep-a-pkg rep-b-pkg))
    rep-b-pkg)

  ;; ----------------------------------------
  ;; Get a package's info, returning its deps and implies:
  (define (get-immediate-pkg-info! pkg)
    (define dir (pkg-directory pkg))
    (unless dir 
      (error 'check-dependencies "package not installed: ~s" pkg))
    ;; Get package information:
    (define-values (checksum mods deps+build-deps)
      (get-pkg-content (pkg-desc (if (path? dir) (path->string dir) dir) 'dir pkg #f #f)
                       #:namespace metadata-ns
                       #:extract-info (lambda (i)
                                        (if (and i
                                                 (or (i 'deps (lambda () #f))
                                                     (i 'build-deps (lambda () #f))))
                                            (cons
                                             (extract-pkg-dependencies i 
                                                                       #:build-deps? #f 
                                                                       #:filter? #t)
                                             (extract-pkg-dependencies i 
                                                                       #:filter? #t))
                                            #f))))
    (unless deps+build-deps
      (hash-set! skip-pkgs pkg #t)
      (setup-printf #f "package declares no dependencies: ~s" pkg))
    (define deps (if deps+build-deps
                     (filter-map package-source->name (cdr deps+build-deps))
                     '()))
    (define runtime-deps (if deps+build-deps
                             (list->set (filter-map package-source->name
                                                    (car deps+build-deps)))
                             (set)))
    (define implies 
      (list->set (let ([i (get-info/full dir #:namespace metadata-ns)])
                   (if i
                       (i 'implies (lambda () null))
                       null))))
    ;; check that `implies' is a subset of `deps'
    (for ([i (in-set implies)])
      (unless (eq? i 'core)
        (unless (set-member? runtime-deps i)
          (setup-fprintf* (current-error-port) #f 
                          (string-append
                           "implied package is not declared as a dependency:\n"
                           " in package: ~s\n"
                           " implied package: ~s\n")
                          pkg
                          i))))
    (for ([mod (in-list mods)])
      (hash-set! mod-pkg mod pkg))
    ;; Save immediate dependencies, initialize external dependencies:
    (hash-set! pkg-reps pkg pkg)
    (hash-set! pkg-immediate-deps pkg (list
                                       (set-add runtime-deps
                                                pkg)
                                       (set-add (list->set deps) 
                                                pkg)))
    (hash-set! pkg-external-deps pkg (set-add (set-intersect
                                               implies
                                               (set-add runtime-deps
                                                        'core))
                                              pkg))
    (when check-unused?
      (hash-set! pkg-implies pkg implies))
    (values deps implies))
  
  ;; ----------------------------------------
  ;; Flatten package dependencies, record mod->pkg mappings,
  ;; return representative package name (of a recursive set)
  (define (register-pkg! pkg ancestors)
    (cond
     [(hash-ref pkg-reps pkg #f)
      => (lambda (rep-pkg) rep-pkg)]
     [else
      (when verbose?
        (setup-printf #f " checking dependencies of ~s" pkg))
      (define-values (deps implies) (get-immediate-pkg-info! pkg))
      ;; Recur on all dependencies
      (define new-ancestors (hash-set ancestors pkg #t))
      (define rep-pkg
        (for/fold ([rep-pkg pkg]) ([dep (in-list deps)])
          (define dep-rep-pkg (register-pkg! dep ancestors))
          (cond
           [(not (set-member? implies dep))
            ;; not implied, so doesn't add external dependencies
            rep-pkg]
           [(equal? dep-rep-pkg rep-pkg)
            ;; an "implies" cycle that points back here - done!
            rep-pkg]
           [(hash-ref ancestors dep-rep-pkg #f)
            ;; an "implies" cycle back to an ancestor; union to ancestor
            (union-find! rep-pkg dep-rep-pkg)]
           [else
            ;; assert: external deps of `dep-rep-pkg' will not change anymore
            (define new-rep-pkg (find-rep! rep-pkg))
            (hash-set! pkg-external-deps
                       rep-pkg
                       (set-union (hash-ref pkg-external-deps dep-rep-pkg)
                                  (hash-ref pkg-external-deps new-rep-pkg)))
            new-rep-pkg])))
      rep-pkg]))

  ;; ----------------------------------------
  ;; Fill in package internal dependencies, given that immediate-dependency
  ;; external-dependency information is available for all relevant packages:
  (define (init-pkg-internals! pkg)
    (unless (hash-ref pkg-internal-deps pkg #f)
      ;; register modules and compute externally visible dependencies
      (register-pkg! pkg (hash))
      ;; combine flattened external dependencies to determine internal dependencies
      (define (flatten imm-deps)
        (for/fold ([deps (set)]) ([dep (in-set imm-deps)])
          (set-union deps
                     (hash-ref pkg-external-deps (find-rep! dep)))))
      (let ([imm-depss (hash-ref pkg-immediate-deps pkg)])
        (hash-set! pkg-internal-deps
                   pkg
                   (map flatten imm-depss))
        (when check-unused?
          (hash-set! pkg-actual-deps
                     pkg
                     (map (lambda (ignored) (make-hash)) imm-depss))))
      (when verbose?
        (define (make-list s)
          (apply
           string-append
           (for/list ([k (in-set s)])
             (format "\n   ~s" k))))
        (setup-printf* #f 
                       (string-append
                        " declared accesses, counting `implies'\n"
                        "  for package: ~s\n"
                        "  packages:~a\n"
                        "  packages for build:~a\n")
                       pkg
                       (make-list (car (hash-ref pkg-internal-deps pkg)))
                       (make-list (cadr (hash-ref pkg-internal-deps pkg)))))))
  
  ;; ----------------------------------------
  ;; Check use of `src-pkg' (in `mode') from `pkg':
  (define (check-dep! pkg src-pkg mode)
    (define flat-depss (hash-ref pkg-internal-deps pkg))
    (when check-unused?
      (define actual-depss (hash-ref pkg-actual-deps pkg))
      (hash-set! (if (eq? mode 'run) (car actual-depss) (cadr actual-depss))
                 src-pkg
                 #t))
    (or (set-member? (if (eq? mode 'run)
                         (car flat-depss)
                         (cadr flat-depss))
                     src-pkg)
        (begin
          (hash-update! missing pkg
                        (lambda (h)
                          (hash-update h src-pkg
                                       (lambda (old-mode)
                                         (if (eq? mode old-mode)
                                             mode
                                             'run))
                                       mode))
                        (hash))
          #f)))

  ;; ----------------------------------------
  ;; Check use of `mod' (in `mode') from `pkg' by file `f':
  (define reported (make-hash))
  (define (check-mod! mod mode pkg f dir)
    (define src-pkg (or (hash-ref mod-pkg mod #f)
                        'core))
    (when src-pkg
      (unless (check-dep! pkg src-pkg mode)
        (define key (list pkg src-pkg (path-replace-suffix f #"") mod))
        (unless (hash-ref reported key #f)
          (hash-set! reported key #t)
          (setup-fprintf* (current-error-port) #f 
                          (string-append
                           "found undeclared dependency:\n"
                           "  mode: ~s\n"
                           "  for package: ~s\n"
                           "  on package: ~s\n"
                           "  dependent source: ~a\n"
                           "  used module: ~s")
                          mode
                          pkg
                          src-pkg
                          (build-path dir f)
                          mod)))))

  
  ;; ----------------------------------------
  (define doc-pkgs (make-hash))
  (define doc-reported (make-hash))
  (define (check-doc! pkg dep dest-dir)
    (define-values (base name dir?) (split-path dep))
    (define src-pkg (hash-ref doc-pkgs base #f))
    (when src-pkg
      (unless (check-dep! pkg src-pkg 'build)
        (define key (list base dest-dir))
        (unless (hash-ref doc-reported key #f)
          (define (get-name p)
            (define-values (b n d?) (split-path p))
            (path-element->string n))
          (hash-set! doc-reported key #t)
          (setup-fprintf* (current-error-port) #f 
                          (string-append
                           "found undeclared dependency:\n"
                           "  mode: build (of documentation)\n"
                           "  for package: ~s\n"
                           "  on package: ~s\n"
                           "  from document: ~s\n"
                           "  to document: ~s")
                          pkg
                          src-pkg
                          (get-name dest-dir)
                          (get-name base))))))

  ;; ----------------------------------------
  (define (check-bytecode-deps f dir coll-path pkg)
    (define zo-f (path-replace-suffix f #".zo"))
    (when (file-exists? (build-path dir zo-f))
      (define base (let ([m (regexp-match #rx#"^(.*)_[a-z]+[.]zo$"
                                          (path-element->bytes zo-f))])
                     (and m (bytes->string/utf-8 (cadr m)))))
      (define in-mod `(lib ,(string-join 
                             (append (map path-element->string coll-path) (list base))
                             "/")))
      (define mod-code (call-with-input-file*
                        (build-path dir zo-f)
                        (lambda (i)
                          (parameterize ([read-accept-compiled #t])
                            (read i)))))
      ;; Recur to cover submodules:
      (let loop ([mod-code mod-code])
        (define name (module-compiled-name mod-code))
        (unless (and (list? name)
                     (memq (last name) build-only-submod-names))
          ;; Check the module's imports:
          (for* ([imports (in-list (module-compiled-imports mod-code))]
                 [import (cdr imports)])
            (define mod (let ([m (collapse-module-path-index import in-mod)])
                          (if (and (pair? m)
                                   (eq? (car m) 'submod))
                              (cadr m)
                              m)))
            (when (and (pair? mod) (eq? 'lib (car mod)))
              (check-mod! mod 'run pkg zo-f dir)))
          ;; Recur for submodules:
          (for-each loop
                    (append
                     (module-compiled-submodules mod-code #t)
                     (module-compiled-submodules mod-code #f)))))))

  ;; ----------------------------------------
  (define main-db-file (build-path (find-doc-dir) "docindex.sqlite"))
  (define user-db-file (build-path (find-user-doc-dir) "docindex.sqlite"))
  (define (register-or-check-docs check? pkg path main?)
    (define db-file (if main? main-db-file user-db-file))
    (when (file-exists? db-file)
      (let ([i (get-info/full path #:namespace metadata-ns)])
        (define scribblings (if i
                                (i 'scribblings (lambda () null))
                                null))
        (for ([s (in-list scribblings)])
          (define src (path->complete-path (car s) path))
          (define name (if ((length s) . > . 3)
                           (list-ref s 3)
                           (path-element->string
                            (path-replace-suffix (file-name-from-path src) #""))))
          (define dest-dir (if main?
                               (build-path (find-doc-dir) name)
                               (build-path path "doc" name)))
          (cond
           [check?
            (for ([dep (in-list (doc-db-get-dependencies (build-path dest-dir "in.sxref") 
                                                         db-file
                                                         #:attach (if main? 
                                                                      #f
                                                                      (and (file-exists? main-db-file)
                                                                           main-db-file))))])
              (check-doc! pkg dep dest-dir))]
           [else
            (hash-set! doc-pkgs (path->directory-path dest-dir) pkg)])))))

  ;; For each collection, set up package info:
  (for ([path (in-list paths)]
        [coll-main? (in-list coll-main?s)])
    (define pkg (path->pkg path #:cache path-cache))
    (when pkg
      (init-pkg-internals! pkg)
      (register-or-check-docs #f pkg path coll-main?)))

  ;; For each collection, check its dependencies:
  (for ([path (in-list paths)]
        [coll-path (in-list coll-paths)]
        [coll-mode (in-list coll-modes)]
        [coll-main? (in-list coll-main?s)])
    (when verbose?
      (setup-printf #f " checking ~a" path))
    (define dir (build-path path "compiled"))
    (when (directory-exists? dir)
      (define pkg (path->pkg dir #:cache path-cache))
      (when (and pkg
                 (not (hash-ref skip-pkgs pkg #f)))
        (for ([f (directory-list dir)])
          ;; A ".dep" file triggers a check:
          (when (regexp-match? #rx#"[.]dep$" (path-element->bytes f))
            ;; Decide whether the file is inherently 'build or 'run mode:
            (define mode
              (if (or (eq? coll-mode 'build)
                      (regexp-match? #rx#"_scrbl[.]dep$" (path-element->bytes f)))
                  'build
                  'run))
            ;; Look at the actual module for 'run mode (dropping
            ;; submodules like `test'):
            (when (eq? mode 'run)
              ;; This is the slowest part, because we have to read the module ".zo"
              (check-bytecode-deps f dir coll-path pkg))
            ;; Treat everything in ".dep" as 'build mode...
            (define deps (cddr (call-with-input-file* (build-path dir f) read)))
            (for ([dep (in-list deps)])
              (when (and (pair? dep)
                         (eq? 'collects (car dep)))
                (define path-strs (map bytes->string/utf-8 (cdr dep)))
                (define mod `(lib ,(string-join path-strs "/")))
                (check-mod! mod 'build pkg f dir)))))
        ;; Treat all (direct) documentation links as 'build mode:
        (register-or-check-docs #t pkg path coll-main?))))

  (when check-unused?
    (for ([(pkg actuals) (in-hash pkg-actual-deps)])
      (define availables (hash-ref pkg-internal-deps pkg))
      (define unused
        (for/hash ([actual (in-list actuals)]
                   [available (in-list availables)]
                   [mode '(run build)]
                   #:when #t
                   [i (in-set available)]
                   #:unless (or (equal? i pkg)
                                (equal? i core-pkg)
                                (equal? i 'core)
                                (hash-ref actual i #f)
                                ;; If `i` is implied, then there's a
                                ;; good reason for the dependency.
                                (set-member? (hash-ref pkg-implies pkg (set)) i)
                                ;; If `i' is implied by a package
                                ;; that is used directly, then there's
                                ;; no way around the dependency, so don't
                                ;; report it.
                                (for/or ([a (in-hash-keys actual)])
                                  (set-member? (hash-ref pkg-implies a (set)) i))))
          ;; note that 'build override 'run
          (values i mode)))
      (unless (zero? (hash-count unused))
        (setup-fprintf (current-error-port) #f 
                       (apply
                        string-append
                        "unused dependenc~a detected\n"
                        "  for package: ~s\n"
                        "  on package~a:"
                        (for/list ([(i mode) (in-hash unused)])
                          (format "\n   ~s~a" 
                                  i
                                  (if (eq? mode 'run)
                                      " for run"
                                      ""))))
                       (if (= (hash-count unused) 1) "y" "ies")
                       pkg
                       (if (= (hash-count unused) 1) "" "s")))))
  
  ;; Report result summary and (optionally) repair:
  (unless (zero? (hash-count missing))
    (setup-fprintf (current-error-port) #f 
                   "--- summary of missing dependencies ---"))
  (for ([pkg (in-list (sort (hash-keys missing) string<?))])
    (define pkgs (hash-ref missing pkg))
    (define modes '(run build))
    (define pkgss (for/list ([mode modes])
                    (sort
                     (for/list ([(pkg pkg-mode) (in-hash pkgs)]
                                #:when (eq? mode pkg-mode))
                       (if (eq? pkg 'core)
                           core-pkg
                           pkg))
                     string<?)))
    (apply setup-fprintf* (current-error-port) #f 
           (apply
            string-append
            "undeclared dependency detected\n"
            "  for package: ~s"
            (for/list ([pkgs (in-list pkgss)]
                       [mode (in-list modes)]
                       #:when (pair? pkgs))
              (format "\n  on package~a~a:~~a"
                      (if (null? (cdr pkgs)) "" "s")
                      (case mode
                        [(run) ""]
                        [(build) " for build"]))))
           pkg
           (for/list ([pkgs (in-list pkgss)]
                      [mode (in-list modes)]
                      #:when (pair? pkgs))
             (apply
              string-append
              (for/list ([k (in-list pkgs)])
                (format "\n   ~s" k)))))
    (when fix?
      (define info-path (build-path (pkg-directory pkg) "info.rkt"))
      (setup-printf #f "repairing ~s..." info-path)
      (fix-info-deps-definition info-path 'deps (car pkgss))
      (fix-info-deps-definition info-path 'build-deps (cadr pkgss))))
  (zero? (hash-count missing)))

(define (fix-info-deps-definition info-path deps-id pkgs)
  (unless (null? pkgs)
    (unless (file-exists? info-path)
      (call-with-output-file* 
       info-path
       (lambda (o)
         (displayln "#lang info" o))))
    (define stx (call-with-input-file* 
                 info-path
                 (lambda (i)
                   (port-count-lines! i)
                   (with-module-reading-parameterization
                    (lambda ()
                      (read-syntax info-path i))))))
    (define deps-stx
      (syntax-case stx ()
        [(mod name lang (#%mb def ...))
         (for/or ([def (in-list (syntax->list #'(def ...)))])
           (syntax-case def ()
             [(dfn id rhs)
              (eq? 'define (syntax-e #'dfn))
              (and (eq? deps-id (syntax-e #'id))
                   def)]
             [_ #f]))]
        [_
         (error 'fix-deps "could not parse ~s" info-path)]))
    (cond
     [deps-stx
      (define (fixup prefix start indent)
        (unless (and start indent)
          (error 'fix-deps
                 "could get relevant source location for `~a' definition in ~s"
                 deps-id
                 info-path))
        (define str (file->string info-path))
        (define new-str
          (string-append (substring str 0 start)
                         (apply
                          string-append
                          (for/list ([s (in-list pkgs)])
                            (format "~a~s\n~a" 
                                    prefix
                                    s
                                    (make-string indent #\space))))
                         (substring str start)))
        (call-with-output-file*
         info-path
         #:exists 'truncate
         (lambda (o) (display new-str o))))
      (define (x+ a b) (and a b (+ a b)))
      (syntax-case deps-stx ()
        [(def id (quot parens))
         (and (eq? 'quote (syntax-e #'quot))
              (or (null? (syntax-e #'parens))
                  (pair? (syntax-e #'parens))))
         (fixup ""
                (syntax-position #'parens)
                (add1 (syntax-column #'parens)))]
        [(def id (lst . elms))
         (eq? 'list (syntax-e #'lst))
         (syntax-case deps-stx ()
           [(_ _ parens)
            (fixup " "
                   (x+ (x+ (syntax-position #'lst)
                           -1)
                       (syntax-span #'lst))
                   (x+ (syntax-column #'lst)
                       (syntax-span #'lst)))])]
        [_
         (error 'fix-deps
                "could parse `~a' definition in ~s"
                deps-id
                info-path)])]
     [else
      (define prefix (format "(define ~a '(" deps-id))
      (call-with-output-file*
       info-path
       #:exists 'append
       (lambda (o)
         (display prefix o)
         (for ([pkg (in-list pkgs)]
               [i (in-naturals)])
           (unless (zero? i)
             (newline o)
             (display (make-string (string-length prefix) #\space) o))
           (write pkg o))
         (displayln "))" o)))])))
