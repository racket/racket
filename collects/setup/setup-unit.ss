
; Expects parameters to be set before invocation.
; Calls `exit' when done.

#lang scheme/base

(require mzlib/unit
         (except-in mzlib/file call-with-input-file* call-with-output-file*)
         mzlib/list
         mzlib/cm
         mzlib/port
         mzlib/match
         mzlib/process
         planet/planet-archives
         planet/private/planet-shared

         "option-sig.ss"
         compiler/sig
         launcher/launcher-sig

         "unpack.ss"
         "getinfo.ss"
         "dirs.ss"
         "main-collects.ss")

(define-namespace-anchor anchor)

(provide setup@)

(define-unit setup@
  (import setup-option^
          compiler^
          (prefix compiler:option: compiler:option^)
          launcher^)
  (export)

  (define (setup-fprintf p s . args)
    (apply fprintf p (string-append "setup-plt: " s "~n") args))

  (define (setup-printf s . args)
    (apply setup-fprintf (current-output-port) s args))

  (setup-printf "Setup version is ~a [~a]" (version) (system-type 'gc))
  (setup-printf "Available variants:~a"
                (apply string-append
                       (map (lambda (s) (format " ~a" s))
                            (available-mzscheme-variants))))
  (setup-printf "Main collection path is ~a" (find-collects-dir))
  (setup-printf "Collection search path is ~a"
                (if (null? (current-library-collection-paths))
                  "empty!" ""))
  (for ([p (current-library-collection-paths)])
    (setup-printf "  ~a" (path->string p)))

  (define (warning s x)
    (setup-printf s (if (exn? x) (exn-message x) x)))

  (define (call-info info flag mk-default test)
    (if info
      (let ([v (info flag mk-default)]) (test v) v)
      (mk-default)))

  (define mode-dir
    (if (compile-mode)
      (build-path "compiled" (compile-mode))
      (build-path "compiled")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   Errors                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errors null)
  (define (record-error cc desc go fail-k)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (if (exn? x)
                         (fprintf (current-error-port) "~a\n" (exn-message x))
                         (fprintf (current-error-port) "~s\n" x))
                       (set! errors (cons (list cc desc x) errors))
                       (fail-k))])
      (go)))
  (define-syntax begin-record-error
    (syntax-rules ()
      [(_ cc desc body ...) (record-error cc desc (lambda () body ...) void)]))
  (define (show-errors port)
    (for ([e (reverse errors)])
      (let ([cc (car e)]
            [desc (cadr e)]
            [x (caddr e)])
        (setup-fprintf port "Error during ~a for ~a"
                       desc
                       (if (cc? cc) 
                           (format "~a (~a)" (cc-name cc) (path->string (cc-path cc)))
                           cc))
        (if (exn? x)
          (setup-fprintf port "  ~a" (exn-message x))
          (setup-fprintf port "  ~s" x)))))

  (define (done)
    (setup-printf "Done setting up")
    (unless (null? errors)
      (setup-printf "")
      (show-errors (current-error-port))
      (when (pause-on-errors)
        (fprintf (current-error-port)
                 "INSTALLATION FAILED.\nPress Enter to continue...\n")
        (read-line))
      (exit 1))
    (exit 0))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Archive Unpacking               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define x-specific-collections
    (apply append
           (specific-collections)
           (map (lambda (x)
                  (unpack x
                          (build-path (find-collects-dir) 'up)
                          (lambda (s) (setup-printf "~a" s))
                          (current-target-directory-getter)
                          (force-unpacks)
                          (current-target-plt-directory-getter)))
                (archives))))

  ;; specific-planet-dir ::=
  ;;    - (list path[directory] string[owner] string[package-name] (listof string[extra package path]) Nat[maj] Nat[min]), or
  ;;    - (list string[owner] string[package-name] string[maj as string] string[min as string])
  ;; x-specific-planet-dir ::= (listof specific-planet-dir)
  (define x-specific-planet-dirs
    (if (make-planet) (specific-planet-dirs) null))

  (define no-specific-collections?
    (and (null? x-specific-collections) (null? x-specific-planet-dirs)))

  (when (and (not (null? (archives))) no-specific-collections?)
    (done)
    (exit 0)) ; done

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;              Find Collections                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-struct cc
    (collection path name info root-dir info-path shadowing-policy)
    #:inspector #f)

  (define (warning-handler v)
    (lambda (exn)
      (setup-printf "Warning: ~a" (if (exn? exn) (exn-message exn) exn))
      v))

  ;; collection->cc : listof path -> cc
  (define (collection->cc collection-p)
    (let* ([root-dir (ormap (lambda (p)
                              (parameterize ([current-library-collection-paths
                                              (list p)])
                                (and (with-handlers ([exn:fail? (lambda (x) #f)])
                                       (apply collection-path collection-p))
                                     p)))
                            (current-library-collection-paths))]
           [info (with-handlers ([exn:fail? (warning-handler #f)])
                   (get-info collection-p))]
           [name (call-info info 'name (lambda () #f)
                   (lambda (x)
                     (when (and x (not (string? x)))
                       (error
                        'setup-plt
                        "'name' result from collection ~e is not a string: ~e"
                        collection-p
                        x))))])
      (and name
           (make-cc collection-p
                    (apply collection-path collection-p)
                    name
                    info
                    root-dir
                    (build-path root-dir "info-domain" "compiled" "cache.ss")
                    ;; by convention, all collections have "version" 1 0. This
                    ;; forces them to conflict with each other.
                    (list (cons 'lib (map path->string collection-p)) 1 0)))))

  ;; remove-falses : listof (union X #f) -> listof X
  ;; returns the non-false elements of l in order
  (define (remove-falses l) (filter values l))

  ;; planet-spec->planet-list : (list string string nat nat) -> (list path string string (listof string) nat nat) | #f
  ;; converts a planet package spec into the information needed to create a cc structure
  (define (planet-spec->planet-list spec)
    (match spec
      [(owner pkg-name maj-str min-str)
       (let ([maj (string->number maj-str)]
             [min (string->number min-str)])
         (unless maj
           (error 'setup-plt "bad major version for PLaneT package: ~e" maj-str))
         (unless min
           (error 'setup-plt "bad minor version for PLaneT package: ~e" min-str))
         (let ([pkg (lookup-package-by-keys owner pkg-name maj min min)])
           (if pkg
             pkg
             (error 'setup-plt "not an installed PLaneT package: (~e ~e ~e ~e)"
                    owner pkg-name maj min))))]
      [_ spec]))

  (define (planet->cc path owner pkg-file extra-path maj min)
    (unless (path? path)
      (error 'planet->cc "non-path when building package ~e" pkg-file))
    (let/ec return
      (let* ([info (with-handlers ([exn:fail? (warning-handler #f)])
                     (get-info/full path))]
             [name (call-info info 'name (lambda () (return #f))
                     (lambda (x)
                       (when (and x (not (string? x)))
                         (error
                          'planet->cc
                          "'name' result from directory ~e is not a string: ~e"
                          path
                          x))))])
        (make-cc #f
                 path
                 name
                 info
                 #f ; don't need root-dir; absolute paths in cache.ss will be ok
                 (get-planet-cache-path)
                 (list `(planet ,owner ,pkg-file ,@extra-path) maj min)))))

  ;; planet-cc->sub-cc : cc (listof bytes [encoded path]) -> cc
  ;; builds a compilation job for the given subdirectory of the given cc this
  ;; is an awful hack
  (define (planet-cc->sub-cc cc subdir)
    (match-let ([(('planet owner pkg-file extra-path ...) maj min)
                 (cc-shadowing-policy cc)])
      (planet->cc (apply build-path (cc-path cc) (map bytes->path subdir))
                  owner
                  pkg-file
                  (append extra-path subdir)
                  maj
                  min)))

  (define (cannot-compile c)
    (error 'setup-plt "don't know how to compile collection: ~a"
           (if (= (length c) 1) (car c) c)))

  (define planet-dirs-to-compile
    (if (make-planet)
      (remove-falses (map (lambda (spec) (apply planet->cc spec))
                          (if no-specific-collections?
                            (get-all-planet-packages)
                            (remove-falses (map planet-spec->planet-list
                                                x-specific-planet-dirs)))))
      null))

  (define all-collections
    (let ([ht (make-hash-table 'equal)])
      (for ([cp (current-library-collection-paths)]
            #:when (directory-exists? cp)
            [collection (directory-list cp)]
            #:when (directory-exists? (build-path cp collection)))
        (hash-table-get ht collection
          (lambda ()
            (let ([cc (collection->cc (list collection))])
              (when cc (hash-table-put! ht collection cc))))))
      (hash-table-map ht (lambda (k v) v))))

  ;; Close over sub-collections
  (define (collection-closure collections-to-compile)
    (let loop ([l collections-to-compile])
      (if (null? l)
        null
        (let* ([cc (car l)]
               [info (cc-info cc)])
          (append
           (map
            (lambda (subcol)
              (or (collection->cc (map string->path subcol))
                  (cannot-compile subcol)))
            (call-info info 'compile-subcollections
              ;; Default: subdirs with info.ss files
              (lambda ()
                (map (lambda (x)
                       (map path->string (append (cc-collection cc) (list x))))
                     (filter (lambda (p)
                               (let ([d (build-path (cc-path cc) p)])
                                 (and (directory-exists? d)
                                      (file-exists? (build-path d "info.ss")))))
                             (directory-list (cc-path cc)))))
              ;; Result checker:
              (lambda (x)
                (unless (and (list? x)
                             (andmap (lambda (x)
                                       (and (list? x)
                                            (andmap (lambda (x)
                                                      (and (path-string? x)
                                                           (relative-path? x)))
                                                    x)))
                                     x))
                  (error "result is not a list of relative path string lists:"
                         x)))))
           (list cc)
           (loop (cdr l)))))))

  (define (same-collection-name? cc-1 cc-2)
    (let ([split (lambda (cc)
                   (apply append
                          (map (lambda (e)
                                 (if (path? e)
                                   (map path-element->string (explode-path e))
                                   (regexp-split #rx"/" e)))
                               (cc-collection cc))))])
      (equal? (split cc-1) (split cc-2))))

  (define (check-again-all given-ccs)
    (define all-collections* (collection-closure all-collections))
    (for ([cc given-ccs])
      (call-with-input-file* (build-path (cc-path cc) "info.ss")
        (lambda (given-info-port)
          (define given-id (port-file-identity given-info-port))
          (for ([found-cc all-collections*]
                #:when (not (same-collection-name? cc found-cc)))
            (call-with-input-file* (build-path (cc-path found-cc) "info.ss")
              (lambda (found-info-port)
                (when (eq? (port-file-identity found-info-port) given-id)
                  (error 'setup-plt
                         "given collection path: ~e refers to the same info file as another path: ~e"
                         (apply build-path (cc-collection cc))
                         (apply build-path (cc-collection found-cc))))))))))
    given-ccs)

  (define collections-to-compile
    (sort (if no-specific-collections?
            all-collections
            (check-again-all
             (map (lambda (c)
                    (or (collection->cc (map string->path c))
                        (cannot-compile c)))
                  x-specific-collections)))
          (lambda (a b) (string-ci<? (cc-name a) (cc-name b)))))

  (set! collections-to-compile (collection-closure collections-to-compile))

  (set! planet-dirs-to-compile
        (let loop ([l planet-dirs-to-compile])
          (if (null? l)
            null
            (let* ([cc (car l)]
                   [info (cc-info cc)])
              (append
               (remove-falses
                (map
                 (lambda (p)
                   (planet-cc->sub-cc
                    cc
                    (cond
                      [(path? p) (list (path->bytes p))]
                      [(and (list? p) (andmap bytes? p)) p]
                      [else (map (λ (s) (path->bytes (string->path s))) p)])))
                 (call-info info 'compile-subcollections
                            (lambda ()
                              (map (λ (p) (list (path->bytes p)))
                                   (filter
                                    (lambda (p)
                                      (let ((d (build-path (cc-path cc) p)))
                                        (and (directory-exists? d)
                                             (file-exists? (build-path d "info.ss")))))
                                    (directory-list (cc-path cc)))))
                            ;; Result checker:
                            (λ (p)
                               (match p
                                 [(((? (λ (v) (or (string? v) (bytes? v)))) ...) ...)
                                  (void)]
                                 [_ (error "result is not a list of lists of strings: " p)])))))
               (list cc)
               (loop (cdr l)))))))

  (define ccs-to-compile (append collections-to-compile planet-dirs-to-compile))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Helpers                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (control-io-apply print-doing f args)
    (if (make-verbose)
      (begin (apply f args) #t)
      (let* ([oop (current-output-port)]
             [printed? #f]
             [on? #f]
             [dir-table (make-hash-table 'equal)]
             [line-accum #""]
             [op (if (verbose)
                   (current-output-port)
                   (open-output-nowhere))]
             [doing-path (lambda (path)
                           (unless printed?
                             (set! printed? #t)
                             (print-doing oop))
                           (unless (verbose)
                             (let ([path (normal-case-path (path-only path))])
                               (unless (hash-table-get dir-table path (lambda () #f))
                                 (hash-table-put! dir-table path #t)
                                 (print-doing oop path)))))])
        (parameterize ([current-output-port op]
                       [compile-notify-handler doing-path])
          (apply f args)
          printed?))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Clean                        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (delete-file/record-dependency path dependencies)
    (when (regexp-match-positions #rx"[.]dep$" (path->bytes path))
      (let ([deps (with-handlers ([exn:fail? (lambda (x) null)])
                    (with-input-from-file path read))])
        (when (and (pair? deps) (list? deps))
          (for ([s (cdr deps)])
            (unless (and (pair? s)
                         (eq? 'ext (car s)))
              (let ([s (main-collects-relative->path s)])
                (when (path-string? s)
                  (hash-table-put! dependencies s #t))))))))
    (delete-file path))

  (define (delete-files-in-directory path printout dependencies)
    (for ([end-path (directory-list path)])
      (let ([path (build-path path end-path)])
        (cond [(directory-exists? path)
               (void)]
              [(file-exists? path)
               (printout)
               (delete-file/record-dependency path dependencies)]
              [else (error 'delete-files-in-directory
                           "encountered ~a, neither a file nor a directory"
                           path)]))))

  (define (clean-collection cc dependencies)
    (begin-record-error cc "Cleaning"
      (let* ([info (cc-info cc)]
             [default (box 'default)]
             [paths (call-info
                     info
                     'clean
                     (lambda ()
                       (list mode-dir
                             (build-path mode-dir "native")
                             (build-path mode-dir "native" (system-library-subpath))))
                     (lambda (x)
                       (unless (or (eq? x default)
                                   (and (list? x) (andmap path-string? x)))
                         (error 'setup-plt "expected a list of path strings for 'clean, got: ~s"
                                x))))]
             [printed? #f]
             [print-message
              (lambda ()
                (unless printed?
                  (set! printed? #t)
                  (setup-printf "Deleting files for ~a at ~a"
                                (cc-name cc) (path->string (cc-path cc)))))])
        (for ([path paths])
          (let ([full-path (build-path (cc-path cc) path)])
            (when (or (file-exists? full-path) (directory-exists? full-path))
              (let ([path (find-relative-path (normalize-path (cc-path cc))
                                              (normalize-path full-path))])
                (let loop ([path path])
                  (let-values ([(base name dir?) (split-path path)])
                    (cond
                      [(path? base)
                       (loop base)]
                      [(eq? base 'relative)
                       (when (eq? name 'up)
                         (error 'clean
                                "attempted to clean files in ~s which is not a subdirectory of ~s"
                                full-path
                                (cc-path cc)))]
                      [else
                       (error 'clean
                              "attempted to clean files in ~s which is not a subdirectory of ~s"
                              full-path
                              (cc-path cc))]))))
              (cond [(directory-exists? full-path)
                     (delete-files-in-directory full-path print-message dependencies)]
                    [(file-exists? full-path)
                     (delete-file/record-dependency full-path dependencies)
                     (print-message)]
                    [else (void)])))))))

  (when (clean)
    (let ([dependencies (make-hash-table 'equal)])
      ;; Main deletion:
      (for ([cc ccs-to-compile]) (clean-collection cc dependencies))
      ;; Unless specific collections were named, also
      ;;  delete .zos for referenced modules and delete
      ;;  info-domain cache
      (when no-specific-collections?
        (setup-printf "Checking dependencies")
        (let loop ([old-dependencies dependencies])
          (let ([dependencies (make-hash-table 'equal)]
                [did-something? #f])
            (hash-table-for-each
             old-dependencies
             (lambda (file _)
               (let-values ([(dir name dir?) (split-path file)])
                 (let* ([zo (build-path dir mode-dir (path-add-suffix name #".zo"))]
                        [dep (build-path dir mode-dir (path-add-suffix name #".dep"))])
                   (when (and (file-exists? dep) (file-exists? zo))
                     (set! did-something? #t)
                     (setup-printf "  deleting ~a" zo)
                     (delete-file/record-dependency zo dependencies)
                     (delete-file/record-dependency dep dependencies))))))
            (when did-something? (loop dependencies))))
        (setup-printf "Clearing info-domain caches")
        (for ([p (current-library-collection-paths)])
          (let ([fn (build-path p "info-domain" "compiled" "cache.ss")])
            (when (file-exists? fn)
              (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
                (with-output-to-file fn void #:exists 'truncate/replace))))))))

  (when (make-zo)
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (define (do-install-part part)
    (when (or (call-install) (and (eq? part 'post) (call-post-install)))
      (for ([cc ccs-to-compile])
        (let/ec k
          (begin-record-error cc (case part
                                   [(pre)     "Early Install"]
                                   [(general) "General Install"]
                                   [(post)    "Post Install"])
            (let ([fn (call-info (cc-info cc)
                        (case part
                          [(pre)     'pre-install-collection]
                          [(general) 'install-collection]
                          [(post)    'post-install-collection])
                        (lambda () (k #f))
                        (lambda (v)
                          (unless (and (path-string? v)
                                       (relative-path? v))
                            (error "result is not a relative path string: " v))
                          (let ([p (build-path (cc-path cc) v)])
                            (unless (file-exists? p)
                              (error "installer file does not exist: " p)))))])
              (let ([installer
                     (with-handlers ([exn:fail? (lambda (exn)
                                                  (error 'setup-plt
                                                         "error loading installer: ~a"
                                                         (if (exn? exn) (exn-message exn) exn)))])
                       (dynamic-require (build-path (cc-path cc) fn)
                                        (case part
                                          [(pre)     'pre-installer]
                                          [(general) 'installer]
                                          [(post)    'post-installer])))])
                (setup-printf "~aInstalling ~a"
                              (case part [(pre) "Pre-"] [(post) "Post-"] [else ""])
                              (cc-name cc))
                (let ([dir (build-path (find-collects-dir) 'up)])
                  (if (procedure-arity-includes? installer 2)
                    (installer dir (cc-path cc))
                    (installer dir))))))))))

  (do-install-part 'pre)

  (define (make-it desc compile-directory get-namespace)
    ;; To avoid polluting the compilation with modules that are already loaded,
    ;; create a fresh namespace before calling this function.
    ;; To avoid keeping modules in memory across collections, pass
    ;; `make-base-namespace' as `get-namespace', otherwise use
    ;; `current-namespace' for `get-namespace'.
    (for ([cc ccs-to-compile])
      (parameterize ([current-namespace (get-namespace)])
        (begin-record-error cc (format "Compiling ~a" desc)
          (unless (control-io-apply
                   (case-lambda
                     [(p)
                      ;; Main "doing something" message
                      (setup-fprintf p "Compiling ~a used by ~a"
                                     desc (cc-name cc))]
                     [(p where)
                      ;; Doing something specifically in "where"
                      (setup-fprintf p "  in ~a"
                                     (path->string
                                      (path->complete-path where
                                                           (cc-path cc))))])
                   compile-directory
                   (list (cc-path cc) (cc-info cc)))
            (setup-printf "No more ~a to compile for ~a"
                          desc (cc-name cc)))))
      (collect-garbage)))

  (define (with-specified-mode thunk)
    (if (not (compile-mode))
      (thunk)
      ;; Use the indicated mode
      (let ([zo-compile (with-handlers ([exn:fail?
                                         (lambda (exn)
                                           (error 'setup-plt
                                                  "error loading compiler for mode ~s: ~s"
                                                  (compile-mode)
                                                  (if (exn? exn)
                                                    (exn-message exn)
                                                    exn)))])
                          (dynamic-require `(lib "zo-compile.ss" ,(compile-mode)) 'zo-compile))]
            [orig-kinds (use-compiled-file-paths)]
            [orig-compile (current-compile)]
            [orig-namespace (namespace-anchor->empty-namespace anchor)])
        (parameterize ([current-namespace (make-base-empty-namespace)]
                       [current-compile zo-compile]
                       [use-compiled-file-paths (list mode-dir)]
                       [current-compiler-dynamic-require-wrapper
                        (lambda (thunk)
                          (parameterize ([current-namespace orig-namespace]
                                         [use-compiled-file-paths orig-kinds]
                                         [current-compile orig-compile])
                            (thunk)))])
          (thunk)))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make zo                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-zo)
    (with-specified-mode
     (lambda ()
       (make-it ".zos"
                (lambda (dir info)
                  ;; Clean up bad .zos:
                  (unless (info 'assume-virtual-sources (lambda () #f))
                    (let ([c (build-path dir "compiled")])
                      (when (directory-exists? c)
                        (let ([ok-zo-files (make-immutable-hash-table
                                            (map (lambda (p)
                                                   (cons (path-add-suffix p #".zo") #t))
                                                 (append (directory-list dir)
                                                         (info 'virtual-sources (lambda () null))))
                                            'equal)])
                          (for ([p (directory-list c)])
                            (when (and (regexp-match #rx#".zo$" (path-element->bytes p))
                                       (not (hash-table-get ok-zo-files p #f)))
                              (setup-fprintf (current-error-port) " deleting ~a" (build-path c p))
                              (delete-file (build-path c p))))))))
                  ;; Make .zos
                  (compile-directory-zos dir info))
                make-base-empty-namespace))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Info-Domain Cache               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-info-domain)
    ;; Each ht maps a collection root dir to an info-domain table. Even when
    ;; `collections-to-compile' is a subset of all collections, we only care
    ;; about those collections that exist in the same root as the ones in
    ;; `collections-to-compile'.
    (let ([ht (make-hash-table 'equal)]
          [ht-orig (make-hash-table 'equal)])
      (for ([cc ccs-to-compile])
        (let* ([domain (with-handlers ([exn:fail? (lambda (x) (lambda () null))])
                         (dynamic-require
                          (build-path (cc-path cc) "info.ss")
                          '#%info-domain))]
               ;; Check whether we have a table for this cc's info-domain cache:
               [t (hash-table-get ht (cc-info-path cc)
                    (lambda ()
                      ;; No table for this root, yet. Build one.
                      (let ([l (let ([p (cc-info-path cc)])
                                 (if (file-exists? p)
                                   (with-handlers ([exn:fail?
                                                    (warning-handler null)])
                                     (with-input-from-file p read))
                                   null))])
                        ;; Convert list to hash table. Incluse only well-formed
                        ;; list elements, and only elements whose corresponding
                        ;; collection exists.
                        (let ([t (make-hash-table 'equal)]
                              [all-ok? #f])
                          (when (list? l)
                            (set! all-ok? #t)
                            (for ([i l])
                              (match i
                                [((? (lambda (a)
                                       (and (bytes? a)
                                            (let ([p (bytes->path a)])
                                              ;; If we have a root directory,
                                              ;; then the path must be relative
                                              ;; to it, otherwise it must be
                                              ;; absolute:
                                              (and (if (cc-root-dir cc)
                                                     (relative-path? p)
                                                     (complete-path? p))
                                                   (file-exists?
                                                    (build-path
                                                     (if (cc-root-dir cc)
                                                       (build-path (cc-root-dir cc) p)
                                                       p)
                                                     "info.ss"))))))
                                     a)
                                  ((? symbol? b) ...)
                                  c
                                  (? integer? d)
                                  (? integer? e))
                                 (hash-table-put! t a (list b c d e))]
                                [_ (set! all-ok? #f)])))
                          ;; Record the table loaded for this collection root
                          ;; in the all-roots table:
                          (hash-table-put! ht (cc-info-path cc) t)
                          ;; If anything in the "cache.ss" file was bad, then
                          ;; claim that the old table was empty, so that we
                          ;; definitely write the new table.
                          (hash-table-put! ht-orig (cc-info-path cc)
                                           (and all-ok? (hash-table-copy t)))
                          t))))])
          ;; Add this collection's info to the table, replacing any information
          ;; already there.
          (hash-table-put! t
                           (path->bytes (if (cc-root-dir cc)
                                          ;; Use relative path:
                                          (apply build-path (cc-collection cc))
                                          ;; Use absolute path:
                                          (cc-path cc)))
                           (cons (domain) (cc-shadowing-policy cc)))))
      ;; Write out each collection-root-specific table to a "cache.ss" file:
      (hash-table-for-each ht
        (lambda (info-path ht)
          (unless (equal? ht (hash-table-get ht-orig info-path))
            (let-values ([(base name must-be-dir?) (split-path info-path)])
              (unless (path? base)
                (error 'make-info-domain "Internal error: cc had invalid info-path: ~s" info-path))
              (make-directory* base)
              (let ([p info-path])
                (setup-printf "Updating ~a" p)
                (with-handlers ([exn:fail? (warning-handler (void))])
                  (with-output-to-file p
                    #:exists 'truncate/replace
                    (lambda ()
                      (write (hash-table-map ht cons))
                      (newline)))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                       Docs                    ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (doc:verbose)
    (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
      (dynamic-require 'setup/scribble 'verbose)))
  (define (doc:setup-scribblings)
    (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
      (dynamic-require 'setup/scribble 'setup-scribblings)))

  (when (make-docs)
    (setup-printf "Building documentation")
    ((doc:verbose) (verbose))
    (with-handlers ([exn:fail? (lambda (exn)
                                 (setup-printf
                                  "Docs failure: ~a"
                                  (if (exn? exn) (exn-message exn) exn)))])
      ((doc:setup-scribblings)
       (if no-specific-collections? #f (map cc-path ccs-to-compile))
       #f
       (not (null? (archives)))
       (lambda (what go alt) (record-error what "Building docs" go alt)))))

  (define (render-pdf file)
    (define cmd
      (format "pdflatex -interaction=batchmode \"~a\" > /dev/null" file))
    (define logfile (path-replace-suffix file #".log"))
    (let loop ([n 0])
      (when (= n 5)
        (error 'render-pdf "didn't get a stable result after ~a runs" n))
      (if (zero? n)
        (setup-printf "running pdflatex on ~a" file)
        (setup-printf "  re-running ~a~a time"
                      (add1 n) (case (add1 n) [(2) 'nd] [(3) 'rd] [else 'th])))
      (unless (system cmd)
        (call-with-input-file logfile
          (lambda (log) (copy-port log (current-error-port))))
        (error 'setup-plt "pdflatex failed"))
      ;; see if we get a "Rerun" note, these seem to come in two flavors
      ;; * Label(s) may have changed. Rerun to get cross-references right.
      ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
      (cond
        [(call-with-input-file logfile
           (lambda (log) (regexp-match? #px#"changed\\.\\s+Rerun" log)))
         (loop (add1 n))]
        [(zero? n)
         (setup-printf "Warning: no \"Rerun\" found in first run of pdflatex for ~a"
                       file)]))
    (path-replace-suffix file #".pdf"))

  (when (doc-pdf-dest)
    (setup-printf "Building PDF documentation (via pdflatex)")
    (let ([dest-dir (path->complete-path (doc-pdf-dest))])
      (unless (directory-exists? dest-dir)
        (make-directory dest-dir))
      (let ([tmp-dir (build-path (find-system-path 'temp-dir)
                                 (format "pltpdfdoc~a" (current-seconds)))])
        (dynamic-wind
          void
          (lambda ()
            (make-directory tmp-dir)
            ((doc:verbose) (verbose))
            ((doc:setup-scribblings)
             (if no-specific-collections? #f (map cc-path ccs-to-compile))
             tmp-dir
             #f
             (lambda (what go alt)
               (record-error what "Building docs" go alt)))
            (parameterize ([current-directory tmp-dir])
              (for ([f (directory-list)]
                    #:when (regexp-match? #rx#"[.]tex$" (path-element->bytes f)))
                (let* ([pdf    (render-pdf f)]
                       [target (build-path dest-dir pdf)])
                  (when (file-exists? target) (delete-file target))
                  (copy-file pdf target)))))
          (lambda ()
            (when (directory-exists? tmp-dir)
              (delete-directory/files tmp-dir)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make Launchers               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-launchers)
    (let ([name-list
           (lambda (l)
             (unless (and (list? l)
                          (andmap (lambda (x) (and (path-string? x) (relative-path? x)))
                                  l))
               (error "result is not a list of relative path strings:" l)))]
          [flags-list
           (lambda (l)
             (unless (and (list? l) (andmap (lambda (fs) (andmap string? fs)) l))
               (error "result is not a list of strings:" l)))]
          [or-f (lambda (f) (lambda (x) (when x (f x))))])
      (for ([cc ccs-to-compile])
        (begin-record-error cc "Launcher Setup"
          (define info (cc-info cc))
          (define (make-launcher kind
                                 launcher-names
                                 launcher-libraries
                                 launcher-flags
                                 program-launcher-path
                                 make-launcher
                                 up-to-date?)
            (define mzlns
              (call-info info launcher-names (lambda () null) name-list))
            (define mzlls
              (call-info info launcher-libraries (lambda () #f) (or-f name-list)))
            (define mzlfs
              (call-info info launcher-flags (lambda () #f) (or-f flags-list)))
            (cond
              [(null? mzlns) (void)]
              [(not (or mzlls mzlfs))
               (unless (null? mzlns)
                 (setup-printf
                  "Warning: ~a launcher name list ~s has no matching library/flags lists"
                  kind mzlns))]
              [(and (or (not mzlls) (= (length mzlns) (length mzlls)))
                    (or (not mzlfs) (= (length mzlns) (length mzlfs))))
               (for-each
                (lambda (mzln mzll mzlf)
                  (let ([p (program-launcher-path mzln)]
                        [aux (list* `(exe-name . ,mzln)
                                    '(framework-root . #f)
                                    '(dll-dir . #f)
                                    `(relative? . ,(not absolute-installation?))
                                    (build-aux-from-path
                                     (build-path (cc-path cc)
                                                 (path-replace-suffix (or mzll mzln) #""))))])
                    (unless (up-to-date? p aux)
                      (setup-printf "Installing ~a~a launcher ~a"
                                    kind (let ([v (current-launcher-variant)])
                                           (if (eq? v (system-type 'gc))
                                             ""
                                             (format " ~a" v)))
                                    (path->string p))
                      (make-launcher
                       (or mzlf
                           (if (cc-collection cc)
                             (list "-l-" (string-append
                                          (apply string-append
                                                 (map (lambda (s)
                                                        (string-append
                                                         (if (path? s)
                                                           (path->string s)
                                                           s)
                                                         "/"))
                                                      (cc-collection cc)))
                                          mzll))
                             (list "-t-" (path->string (build-path (cc-path cc) mzll)))))
                       p
                       aux))))
                mzlns
                (or mzlls (map (lambda (_) #f) mzlns))
                (or mzlfs (map (lambda (_) #f) mzlns)))]
              [else
               (let ([fault (if (or (not mzlls) (= (length mzlns) (length mzlls))) 'f 'l)])
                 (setup-printf
                  "Warning: ~a launcher name list ~s doesn't match ~a list; ~s"
                  kind mzlns
                  (if (eq? 'l fault) "library" "flags")
                  (if (eq? fault 'l) mzlls mzlfs)))]))
          (for ([variant (available-mred-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher "MrEd"
                             'mred-launcher-names
                             'mred-launcher-libraries
                             'mred-launcher-flags
                             mred-program-launcher-path
                             make-mred-launcher
                             mred-launcher-up-to-date?)))
          (for ([variant (available-mzscheme-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher "MzScheme"
                             'mzscheme-launcher-names
                             'mzscheme-launcher-libraries
                             'mzscheme-launcher-flags
                             mzscheme-program-launcher-path
                             make-mzscheme-launcher
                             mzscheme-launcher-up-to-date?)))))))

  (do-install-part 'general)
  (do-install-part 'post)

  (done))
