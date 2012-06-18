;; Expects parameters to be set before invocation.
;; Calls `exit' when done.

#lang racket/base

(require racket/unit
         racket/path
         racket/file
         racket/port
         racket/match
         racket/system
         racket/list
         racket/string
         compiler/cm
         planet/planet-archives
         planet/private/planet-shared

         "option-sig.rkt"
         compiler/sig
         launcher/launcher-sig
         dynext/dynext-sig

         "unpack.rkt"
         "getinfo.rkt"
         "dirs.rkt"
         "main-collects.rkt"
         "path-to-relative.rkt"
         "path-relativize.rkt"
         "private/omitted-paths.rkt"
         "parallel-build.rkt"
         "collects.rkt"
         "link.rkt")

(define-namespace-anchor anchor)

;; read info files using whatever namespace, .zo-use, and compilation
;;  configuration was in place for loading setup, instead of whatever
;;  is in place for the collections that setup is processing:
(define getinfo
  (let ([ns (namespace-anchor->empty-namespace anchor)]
        [compile (current-compile)]
        [loader (current-load/use-compiled)]
        [paths (use-compiled-file-paths)])
    (lambda (path)
      (parameterize ([current-namespace ns]
                     [current-compile compile]
                     [current-load/use-compiled loader]
                     [use-compiled-file-paths paths])
        (get-info/full path #:namespace ns)))))

(provide setup@)

(define-unit setup@
  (import setup-option^
          compiler^
          dynext:file^
          (prefix compiler:option: compiler:option^)
          launcher^)
  (export)

  (define name-str (setup-program-name))
  (define name-sym (string->symbol name-str))
  (define main-collects-dir (find-collects-dir))
  (define mode-dir
    (if (compile-mode)
      (build-path "compiled" (compile-mode))
      (build-path "compiled")))

  (unless (make-user)
    (current-library-collection-paths
     (if (member main-collects-dir (current-library-collection-paths))
       (list main-collects-dir)
       '())))

  (current-library-collection-paths
   (map simple-form-path (current-library-collection-paths)))

  (define (setup-fprintf p task s . args)
    (let ([task (if task (string-append task ": ") "")])
      (apply fprintf p (string-append name-str ": " task s "\n") args)))

  (define (setup-printf task s . args)
    (apply setup-fprintf (current-output-port) task s args))

  (define (exn->string x) (if (exn? x) (exn-message x) (format "~s" x)))

  ;; auto-curried list-of
  (define list-of
    (case-lambda [(pred) (lambda (x) (and (list? x) (andmap pred x)))]
                 [(pred x) ((list-of pred) x)]))

  (define (relative-path-string? x) (and (path-string? x) (relative-path? x)))

  (define (call-info info flag mk-default test)
    (let ([v (info flag mk-default)]) (test v) v))

  (define path->relative-string/console-bin
    (make-path->relative-string
     (list (cons find-console-bin-dir "<console-bin>/"))))
  (define path->relative-string/gui-bin
    (make-path->relative-string
     (list (cons find-gui-bin-dir "<gui-bin>/"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   Errors                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errors null)
  (define (append-error cc desc exn out err type)
    (set! errors (cons (list cc desc exn out err type) errors)))
  (define (handle-error cc desc exn out err type)
      (if (verbose)
          ((error-display-handler)
           (format "~a\n" (exn->string exn))
           exn)
          (eprintf "~a\n" (exn->string exn)))
      (append-error cc desc exn out err type))
  (define (record-error cc desc go fail-k)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (handle-error cc desc x "" "" "error")
                       (fail-k))])
      (go)))
  (define-syntax begin-record-error
    (syntax-rules ()
      [(_ cc desc body ...) (record-error cc desc (lambda () body ...) void)]))
  (define (show-errors port)
    (for ([e (reverse errors)])
      (match-let ([(list cc desc x out err type) e])
        (setup-fprintf port type "during ~a for ~a" desc (if (cc? cc) (cc-name cc) cc))
        (unless (null? x) (for ([str (in-list (regexp-split #rx"\n" (exn->string x)))])
                            (setup-fprintf port #f "  ~a" str)))
        (unless (zero? (string-length out)) (eprintf "STDOUT:\n~a=====\n" out))
        (unless (zero? (string-length err)) (eprintf "STDERR:\n~a=====\n" err)))))

  (define (done)
    (unless (null? errors)
      (setup-printf #f "")
      (show-errors (current-error-port))
      (when (pause-on-errors)
        (eprintf "INSTALLATION FAILED.\nPress Enter to continue...\n")
        (read-line))
      (exit 1))
    (exit 0))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Archive Unpacking               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define x-specific-collections
    (append* (specific-collections)
             (for/list ([x (in-list (archives))])
               (unpack x
                       (build-path main-collects-dir 'up)
                       (lambda (s) (setup-printf #f "~a" s))
                       (current-target-directory-getter)
                       (force-unpacks)
                       (current-target-plt-directory-getter)))))

  ;; specific-planet-dir ::=
  ;;    - (list path[directory] string[owner] string[package-name] (listof string[extra package path]) Nat[maj] Nat[min]), or
  ;;    - (list string[owner] string[package-name] string[maj as string] string[min as string])
  ;; x-specific-planet-dir ::= (listof specific-planet-dir)
  (define x-specific-planet-dirs
    (if (make-planet) (specific-planet-dirs) null))

  (define no-specific-collections?
    (and (null? x-specific-collections) (null? x-specific-planet-dirs)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;              Find Collections                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-cc* collection path omit-root info-root info-path info-path-mode shadowing-policy)
    (define info
      (or (with-handlers ([exn:fail? (warning-handler #f)]) (getinfo path))
          (lambda (flag mk-default) (mk-default))))
    (define name
      (call-info
       info 'name (lambda () #f)
       (lambda (x)
         (when (and x (not (string? x)))
           (error name-sym
                  "'name' result from collection ~e is not a string: ~e"
                  path x)))))
    (define path-name (path->relative-string/setup path))
    (when (info 'compile-subcollections (lambda () #f))
      (setup-printf "WARNING"
                    "ignoring `compile-subcollections' entry in info ~a"
                    path-name))
    ;; this check is also done in compiler/compiler-unit, in compile-directory
    (and (not (eq? 'all (omitted-paths path getinfo omit-root)))
         (make-cc collection path
                  (if name
                      (format "~a (~a)" path-name name)
                      path-name)
                  info
                  omit-root
                  info-root info-path info-path-mode
                  shadowing-policy)))

  (define ((warning-handler v) exn)
    (setup-printf "WARNING" "~a" (exn->string exn))
    v)

  ;; Maps a colletion name to a list of `cc's:
  (define collection-ccs-table (make-hash))

  ;; collection-cc! : listof-path .... -> cc
  (define (collection-cc! collection-p
                          #:path [dir (apply collection-path collection-p)]
                          #:omit-root [omit-root #f]
                          #:info-root [given-info-root #f]
                          #:info-path [info-path #f]
                          #:info-path-mode [info-path-mode 'relative])
    (define info-root
      (or given-info-root
          (ormap (lambda (p)
                   (parameterize ([current-library-collection-paths (list p)]
                                  ;; to disable collection links file:
                                  [use-user-specific-search-paths #f])
                     (and (with-handlers ([exn:fail? (lambda (x) #f)])
                            (apply collection-path collection-p))
                          p)))
                 (current-library-collection-paths))))
    (unless (directory-exists? dir)
      (error name-sym "directory: ~e does not exist for collection: ~s"
             dir
             (string-join (map path->string collection-p) "/")))
    (unless info-root
      (error name-sym "cannot find info root for collection: ~s and path: ~e"
             (string-join (map path->string collection-p) "/")
             dir))
    (define new-cc
      (make-cc* collection-p
                dir
                (if (eq? omit-root 'dir)
                    dir
                    omit-root) ; #f => `omitted-paths' can reconstruct it
                info-root
                (or info-path
                    (build-path info-root "info-domain" "compiled" "cache.rktd"))
                info-path-mode
                ;; by convention, all collections have "version" 1 0. This
                ;; forces them to conflict with each other.
                (list (cons 'lib (map path->string collection-p)) 1 0)))
    (when new-cc
      (hash-update! collection-ccs-table
                    collection-p
                    (lambda (lst) (cons new-cc lst))
                    null))
    new-cc)

  ;; collection->ccs : listof-path -> listof-cc
  (define (collection->ccs collection-p)
    (hash-ref collection-ccs-table collection-p null))

  ;; planet-spec->planet-list : (list string string nat nat) -> (list path string string (listof string) nat nat) | #f
  ;; converts a planet package spec into the information needed to create a cc structure
  (define (planet-spec->planet-list spec)
    (match spec
      [(list owner pkg-name maj-str min-str)
       (define maj
         (or (string->number maj-str)
             (error name-sym "bad major version for PLaneT package: ~e" maj-str)))
       (define min
         (or (string->number min-str)
             (error name-sym "bad minor version for PLaneT package: ~e" min-str)))
       (or (lookup-package-by-keys owner pkg-name maj min min)
           (error name-sym "not an installed PLaneT package: (~e ~e ~e ~e)"
                  owner pkg-name maj min))]
      [_ spec]))

  (define (planet-cc! path #:omit-root [omit-root path] owner pkg-file extra-path maj min)
    (unless (path? path)
      (error 'planet-cc! "non-path when building package ~e" pkg-file))
    (and (directory-exists? path)
         (make-cc* #f
                   path
                   omit-root
                   #f ; don't need info-root; absolute paths in cache.rktd will be ok
                   (get-planet-cache-path)
                   'abs
                   (list `(planet ,owner ,pkg-file ,@extra-path) maj min))))

  ;; planet-cc->sub-cc : cc (listof bytes [encoded path]) -> cc
  ;; builds a compilation job for the given subdirectory of the given cc this
  ;; is an awful hack
  (define (planet-cc->sub-cc cc subdir)
    (match-let ([(list (list 'planet owner pkg-file extra-path ...) maj min)
                 (cc-shadowing-policy cc)])
      (planet-cc! (apply build-path (cc-path cc) (map bytes->path subdir))
                  #:omit-root (cc-omit-root cc)
                  owner
                  pkg-file
                  (append extra-path subdir)
                  maj
                  min)))

  ;; Add in all non-planet collections:
  (for ([cp (current-library-collection-paths)]
        #:when (directory-exists? cp)
        [collection (directory-list cp)]
        #:when (directory-exists? (build-path cp collection)))
    (collection-cc! (list collection)
                    #:path (build-path cp collection)))
  (let ([main-collects (find-collects-dir)])
    (define (cc! col #:path path)
      (collection-cc! col
                      #:path path
                      #:info-root main-collects
                      #:info-path-mode 'abs-in-relative
                      #:omit-root 'dir))
    (for ([c+p (in-list (links #:user? #f #:with-path? #t))])
      (cc! (list (string->path (car c+p)))
           #:path (cdr c+p)))
    (for ([cp (in-list (links #:root? #t #:user? #f))]
          #:when (directory-exists? cp)
          [collection (directory-list cp)]
          #:when (directory-exists? (build-path cp collection)))
      (cc! (list collection)
           #:path (build-path cp collection))))
  (when (make-user)
    (define user-collects (find-user-collects-dir))
    (define (cc! col #:path path)
      (unless user-collects
        (error name-sym "cannot setup linked collection without a user-collection root"))
      (collection-cc! col
                      #:path path
                      #:info-root user-collects
                      #:info-path-mode 'abs-in-relative
                      #:omit-root 'dir))
    (for ([c+p (in-list (links #:with-path? #t))])
      (cc! (list (string->path (car c+p)))
           #:path (cdr c+p)))
    (for ([cp (in-list (links #:root? #t))]
          #:when (directory-exists? cp)
          [collection (directory-list cp)]
          #:when (directory-exists? (build-path cp collection)))
      (cc! (list collection) #:path (build-path cp collection))))

  ;; `all-collections' lists all top-level collections (not from Planet):
  (define all-collections
    (apply append (hash-map collection-ccs-table (lambda (k v) v))))

  ;; Close over sub-collections
  (define (collection-closure collections-to-compile make-subs)
    (define (get-subs cc)
      (define info (cc-info cc))
      (define ccp (cc-path cc))
      ;; note: omit can be 'all, if this happens then this collection
      ;; should not have been included, but we might jump in if a
      ;; command-line argument specified a coll/subcoll
      (define omit (omitted-paths ccp getinfo (cc-omit-root cc)))
      (define subs (if (eq? 'all omit)
                     '()
                     (filter (lambda (p)
                               (and (directory-exists? (build-path ccp p))
                                    (not (member p omit))))
                             (directory-list ccp))))
      (filter values (make-subs cc subs)))
    (filter values
            (let loop ([l collections-to-compile])
              (append-map (lambda (cc) (cons cc (loop (get-subs cc)))) l))))

  (define (collection-tree-map collections-to-compile
                               #:skip-path [orig-skip-path (and (avoid-main-installation)
                                                                (find-collects-dir))])
    (define skip-path
      (and orig-skip-path
           (path->bytes (simplify-path (if (string? orig-skip-path)
                                         (string->path orig-skip-path)
                                         orig-skip-path)
                                       #f))))
    (define (skip-path? path)
      (and skip-path
           (let ([b (path->bytes (simplify-path path #f))]
                 [len (bytes-length skip-path)])
             (and ((bytes-length b) . > . len)
                  (bytes=? (subbytes b 0 len) skip-path)))
           path))

    (define (build-collection-tree cc)
      (define (make-child-cc parent-cc name)
        (collection-cc! (append (cc-collection parent-cc) (list name))
                        #:info-root (cc-info-root cc)
                        #:info-path (cc-info-path cc)
                        #:info-path-mode (cc-info-path-mode cc)
                        #:omit-root (cc-omit-root cc)))
      (define info (cc-info cc))
      (define ccp  (cc-path cc))
      ;; note: omit can be 'all, if this happens then this collection
      ;; should not have been included, but we might jump in if a
      ;; command-line argument specified a coll/subcoll
      (define omit (omitted-paths ccp getinfo (cc-omit-root cc)))
      (define-values [dirs files]
        (if (eq? 'all omit)
            (values null null)
            (partition (lambda (x) (directory-exists? (build-path ccp x)))
                       (filter (lambda (p)
                                 (not (or (member p omit)
                                          (skip-path? p))))
                               (directory-list ccp)))))
      (define children-ccs
        (map build-collection-tree
             (filter-map (lambda (x) (make-child-cc cc x)) dirs)))
      (define srcs
        (append
         (filter extract-base-filename/ss files)
         (if (make-docs)
           (filter (lambda (p) (not (member p omit)))
                   (map (lambda (s) (if (string? s) (string->path s) s))
                        (map car (call-info info 'scribblings
                                            (lambda () null) (lambda (x) #f)))))
           null)))
      (list cc srcs children-ccs))
    (map build-collection-tree collections-to-compile))

  (define (plt-collection-closure collections-to-compile)
    (define (make-children-ccs cc children)
      (map (lambda (child)
             (collection-cc! (append (cc-collection cc) (list child))
                             #:path (build-path (cc-path cc) child)
                             #:info-root (cc-info-root cc)
                             #:info-path (cc-info-path cc)
                             #:info-path-mode (cc-info-path-mode cc)
                             #:omit-root (cc-omit-root cc)))
           children))
    (collection-closure collections-to-compile make-children-ccs))

  (define (lookup-collection-closure collections-to-compile)
    (define (lookup-children-ccs cc children)
      (apply
       append
       (map (lambda (child)
              (collection->ccs (append (cc-collection cc) (list child))))
            children)))
    (collection-closure collections-to-compile lookup-children-ccs))

  (define all-collections-closure (plt-collection-closure all-collections))

  (define (check-against-all given-ccs nothing-else-to-do?)
    (when (and (null? given-ccs)
               nothing-else-to-do?)
      (setup-printf #f "nothing to do")
      (exit 1))
    (define (cc->name cc)
      (string-join (map path->string (cc-collection cc)) "/"))
    (define (cc->cc+name+id cc)
      (list cc (cc->name cc) (file-or-directory-identity (cc-path cc))))
    (define all-ccs+names+ids
      (map cc->cc+name+id all-collections-closure))
    ;; given collections
    (define given-ccs+names+ids (map cc->cc+name+id given-ccs))
    ;; descendants of given collections
    (define descendants-names
      (remove-duplicates
       (append-map
        (lambda (cc)
          (map cc->name (remq cc (lookup-collection-closure (list cc)))))
        given-ccs)))
    ;; given collections without duplicates and without ones that are already
    ;; descendants
    (define given*-ccs+names+ids
      (remove-duplicates
       (filter (lambda (cc+name+id)
                 (not (member (cadr cc+name+id) descendants-names)))
               given-ccs+names+ids)
       (lambda (x y)
         (and (equal? (cadr x) (cadr y))
              (equal? (cc-path (car x)) (cc-path (car y)))))))
    ;; check that there are no bad duplicates in the given list
    (for ([given-cc+name+id (in-list given*-ccs+names+ids)])
      (define bad
        (ormap (lambda (cc+name+id)
                 (and (not (equal? (cadr cc+name+id) (cadr given-cc+name+id)))
                      (equal? (caddr cc+name+id) (caddr given-cc+name+id))
                      (cadr cc+name+id)))
               all-ccs+names+ids))
      (when bad
        (error name-sym
               "given collection path: \"~a\" refers to the same directory as another given collection path, \"~a\""
               (cadr given-cc+name+id) bad)))
    (map car given*-ccs+names+ids))

  (define (sort-collections ccs)
    (sort ccs string<? #:key cc-name))

  (define (sort-collections-tree ccs)
    (sort ccs string<? #:key (lambda (x) (cc-name (first x)))))

  (define planet-collects
    (if (make-planet)
      (filter-map (lambda (spec) (apply planet-cc! spec))
                  (if no-specific-collections?
                    (get-all-planet-packages)
                    (filter-map planet-spec->planet-list
                                x-specific-planet-dirs)))
      null))

  (define top-level-plt-collects
    (if no-specific-collections?
      all-collections
      (check-against-all
       (append-map
        (lambda (c)
          (define elems
            (append-map (lambda (s) (map string->path (regexp-split #rx"/" s)))
                        c))
          (define ccs (collection->ccs elems))
          (when (null? ccs)
            ;; let `collection-path' complain about the name, if that's the problem:
            (with-handlers ([exn? (compose1 raise-user-error exn-message)])
              (apply collection-path elems))
            ;; otherwise, it's probably a collection with nothing to compile
            ;; spell the name
            (setup-printf "WARNING"
                          "nothing to compile in a given collection path: \"~a\""
                          (string-join c "/")))
          ccs)
        x-specific-collections)
       (null? planet-collects))))

  (define planet-dirs-to-compile
    (sort-collections
      (collection-closure
        planet-collects
        (lambda (cc subs)
          (map (lambda (p) (planet-cc->sub-cc cc (list (path->bytes p))))
               subs)))))

  (define ccs-to-compile
    (append
     (sort-collections (lookup-collection-closure top-level-plt-collects))
     planet-dirs-to-compile))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Clean                        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (delete-file/record-dependency path dependencies)
    (when (regexp-match-positions #rx"[.]dep$" (path->bytes path))
      (define deps
        (with-handlers ([exn:fail? (lambda (x) null)])
          (with-input-from-file path read)))
      (when (and (pair? deps) (list? deps))
        (for ([s (in-list (cddr deps))])
          (unless (and (pair? s) (eq? 'ext (car s)))
            (define new-s (main-collects-relative->path s))
            (when (path-string? new-s) (hash-set! dependencies new-s #t))))))
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
      (define info (cc-info cc))
      (define paths
        (call-info
         info
         'clean
         (lambda ()
           (list mode-dir
                 (build-path mode-dir "native")
                 (build-path mode-dir "native" (system-library-subpath))))
         (lambda (x)
           (unless (list-of path-string? x)
             (error name-sym
                    "expected a list of path strings for 'clean, got: ~s"
                    x)))))
      (define printed? #f)
      (define (print-message)
        (unless printed?
          (set! printed? #t)
          (setup-printf "deleting" "in ~a"
                        (path->relative-string/setup (cc-path cc)))))
      (for ([path paths])
        (define full-path (build-path (cc-path cc) path))
        (when (or (file-exists? full-path) (directory-exists? full-path))
          (let loop ([path (find-relative-path (simple-form-path (cc-path cc))
                                               (simple-form-path full-path))])
            (define-values [base name dir?] (split-path path))
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
                      (cc-path cc))]))
          (cond [(directory-exists? full-path)
                 (delete-files-in-directory full-path print-message dependencies)]
                [(file-exists? full-path)
                 (delete-file/record-dependency full-path dependencies)
                 (print-message)]
                [else (void)])))))

  (define (clean-step)
    (setup-printf #f "--- cleaning collections ---")
    (define dependencies (make-hash))
    ;; Main deletion:
    (for ([cc ccs-to-compile]) (clean-collection cc dependencies))
    ;; Unless specific collections were named, also delete .zos for
    ;; referenced modules and delete info-domain cache
    (when no-specific-collections?
      (setup-printf #f "checking dependencies")
      (let loop ([old-dependencies dependencies])
        (define dependencies (make-hash))
        (define did-something? #f)
        (hash-for-each
         old-dependencies
         (lambda (file _)
           (define-values [dir name dir?] (split-path file))
           (define zo  (build-path dir mode-dir (path-add-suffix name #".zo")))
           (define dep (build-path dir mode-dir (path-add-suffix name #".dep")))
           (when (and (file-exists? dep) (file-exists? zo))
             (set! did-something? #t)
             (setup-printf "deleting" "~a" (path->relative-string/setup zo))
             (delete-file/record-dependency zo dependencies)
             (delete-file/record-dependency dep dependencies))))
        (when did-something? (loop dependencies)))
      (setup-printf #f "clearing info-domain caches")
      (for ([p (current-library-collection-paths)])
        (define fn (build-path p "info-domain" "compiled" "cache.rktd"))
        (when (file-exists? fn)
          (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
            (with-output-to-file fn void #:exists 'truncate/replace))))))

  (define (do-install-part part)
    (when (if (eq? part 'post) (call-post-install) (call-install))
      (setup-printf #f (format "--- ~ainstalling collections ---"
                               (case part
                                 [(pre) "pre-"]
                                 [(general) ""]
                                 [(post) "post-"])))
      (for ([cc ccs-to-compile])
        (let/ec k
          (begin-record-error cc (case part
                                   [(pre)     "Early Install"]
                                   [(general) "General Install"]
                                   [(post)    "Post Install"])
            (define fn
              (call-info (cc-info cc)
                (case part
                  [(pre)     'pre-install-collection]
                  [(general) 'install-collection]
                  [(post)    'post-install-collection])
                (lambda () (k #f))
                (lambda (v)
                  (unless (relative-path-string? v)
                    (error "result is not a relative path string: " v))
                  (define p (build-path (cc-path cc) v))
                  (unless (file-exists? p)
                    (error "installer file does not exist: " p)))))
            (define installer
              (with-handlers ([exn:fail?
                               (lambda (exn)
                                 (error name-sym
                                        "error loading installer: ~a"
                                        (exn->string exn)))])
                (dynamic-require (build-path (cc-path cc) fn)
                                 (case part
                                   [(pre)     'pre-installer]
                                   [(general) 'installer]
                                   [(post)    'post-installer]))))
            (setup-printf (format "~ainstalling"
                                  (case part
                                    [(pre) "pre-"]
                                    [(post) "post-"]
                                    [else ""]))
                          "~a"
                          (cc-name cc))
            (define dir (build-path main-collects-dir 'up))
            (if (procedure-arity-includes? installer 2)
                (installer dir (cc-path cc))
                (installer dir)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make zo                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (control-io print-verbose thunk)
    (if (make-verbose)
      (thunk)
      (let* ([oop (current-output-port)]
             [dir-table (make-hash)]
             [doing-path (lambda (path)
                           (unless (verbose)
                             (let ([path (path-only path)])
                               (unless (hash-ref dir-table path #f)
                                 (hash-set! dir-table path #t)
                                 (print-verbose oop path)))))])
        (parameterize ([current-output-port (if (verbose) (current-output-port) (open-output-nowhere))]
                       [compile-notify-handler doing-path])
          (thunk)))))

  (define (clean-cc dir info)
    ;; Clean up bad .zos:
    (unless (info 'assume-virtual-sources (lambda () #f))
      (define c (build-path dir "compiled"))
      (when (directory-exists? c)
        (define ok-zo-files
          (make-immutable-hash
           (map (lambda (p)
                  (cons (path-add-suffix p #".zo") #t))
                (append (directory-list dir)
                        (info 'virtual-sources (lambda () null))))))
        (for ([p (directory-list c)])
          (when (and (regexp-match #rx#".(zo|dep)$" (path-element->bytes p))
                     (not (hash-ref ok-zo-files (path-replace-suffix p #".zo") #f)))
            (setup-fprintf (current-error-port) #f " deleting ~a" (build-path c p))
            (delete-file (build-path c p)))))))

  (define (with-specified-mode thunk)
    (if (not (compile-mode))
      (thunk)
      ;; Use the indicated mode
      (let ([zo-compile
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (error name-sym
                                       "error loading compiler for mode ~s: ~a"
                                       (compile-mode)
                                       (exn->string exn)))])
               (dynamic-require `(lib "zo-compile.rkt" ,(compile-mode))
                                'zo-compile))]
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

  ;; We keep timestamp information for all files that we try to compile.
  ;; That's O(N) for an installation of size N, but the constant is small,
  ;; and it makes a do-nothing setup complete much faster.
  (define caching-managed-compile-zo (make-caching-managed-compile-zo))

  (define (compile-cc cc gcs)
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (begin-record-error cc "making"
        (setup-printf "making" "~a" (cc-name cc))
        (control-io
         (lambda (p where)
            (set! gcs 2)
            (setup-fprintf p #f " in ~a"
                           (path->relative-string/setup
                            (path->complete-path where (cc-path cc)))))
         (lambda ()
           (define dir  (cc-path cc))
           (define info (cc-info cc))
           (clean-cc dir info)
           (compile-directory-zos dir info
                                  #:omit-root (cc-omit-root cc)
                                  #:managed-compile-zo caching-managed-compile-zo
                                  #:skip-path (and (avoid-main-installation) (find-collects-dir))
                                  #:skip-doc-sources? (not (make-docs)))))))
    (if (eq? 0 gcs)
        0
        (begin (collect-garbage) (sub1 gcs))))

  ;; To avoid polluting the compilation with modules that are already loaded,
  ;; create a fresh namespace before calling this function.
  ;; To avoid keeping modules in memory across collections, pass
  ;; `make-base-namespace' as `get-namespace', otherwise use
  ;; `current-namespace' for `get-namespace'.
  (define (iterate-cct thunk cct)
    (let loop ([cct cct])
      (map (lambda (x) (thunk (first x)) (loop (third x))) cct)))

  (define (make-zo-step)
    (define (partition-cct name cct)
      (partition (lambda (x) (not (string=? (cc-name (car x)) name))) cct))
    (define (move-to where names cct)
      (for/fold ([cct cct]) ([name (in-list (reverse names))])
        (define-values [diff same] (partition-cct name cct))
        (case where
          [(beginning) (append same diff)]
          [(end) (append diff same)])))
    (setup-printf #f "--- compiling collections ---")
    (if ((parallel-workers) . > . 1)
      (begin
        (for/fold ([gcs 0]) ([cc (in-list (collection->ccs (list (string->path "racket"))))])
          (compile-cc cc 0))
        (managed-compile-zo (collection-file-path "parallel-build-worker.rkt" "setup"))
        (with-specified-mode
          (lambda ()
            (define cct
              (move-to 'beginning (list "compiler" "raco" "racket" "images")
                       (move-to 'end (list "drracket" "drscheme")
                                (sort-collections-tree
                                 (collection-tree-map top-level-plt-collects)))))
            (iterate-cct (lambda (cc)
                           (define dir (cc-path cc))
                           (define info (cc-info cc))
                           (clean-cc dir info))
                         cct)
            (parallel-compile (parallel-workers) setup-fprintf handle-error cct)
            (for/fold ([gcs 0]) ([cc planet-dirs-to-compile])
              (compile-cc cc gcs)))))
      (with-specified-mode
        (lambda ()
          (for/fold ([gcs 0]) ([cc ccs-to-compile])
            (compile-cc cc gcs))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Info-Domain Cache               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-info-domain-step)
    (setup-printf #f "--- updating info-domain tables ---")
    ;; Each ht maps a collection root dir to an info-domain table. Even when
    ;; `collections-to-compile' is a subset of all collections, we only care
    ;; about those collections that exist in the same root as the ones in
    ;; `collections-to-compile'.
    (define ht (make-hash))
    (define ht-orig (make-hash))
    (define roots (make-hash))
    (for ([cc ccs-to-compile])
      (define-values [path->info-relative info-relative->path]
        (apply values
               (hash-ref roots
                         (cc-info-root cc)
                         (lambda ()
                           (define-values [p-> ->p]
                             (if (cc-info-root cc)
                                 (make-relativize (lambda () (cc-info-root cc))
                                                  'info
                                                  'path->info-relative
                                                  'info-relative->path)
                                 (values #f #f)))
                           (hash-set! roots (cc-info-root cc) (list p-> ->p))
                           (list p-> ->p)))))
      (define domain
        (with-handlers ([exn:fail? (lambda (x) (lambda () null))])
          (dynamic-require (build-path (cc-path cc) "info.rkt")
                           '#%info-domain)))
      ;; Check whether we have a table for this cc's info-domain cache:
      (define t
        (hash-ref ht (cc-info-path cc)
          (lambda ()
            ;; No table for this root, yet. Build one.
            (define l
              (let ([p (cc-info-path cc)])
                (if (file-exists? p)
                    (with-handlers ([exn:fail? (warning-handler null)])
                      (with-input-from-file p read))
                    null)))
            ;; Convert list to hash table. Include only well-formed
            ;; list elements, and only elements whose corresponding
            ;; collection exists.
            (define t (make-hash))
            (define all-ok? #f)
            (when (list? l)
              (set! all-ok? #t)
              (for ([i l])
                (match i
                  [(list (and a (or (? bytes?) (list 'info (? bytes?) ...)))
                         (list (? symbol? b) ...) c (? integer? d) (? integer? e))
                   (define p (if (bytes? a) (bytes->path a) a))
                   ;; Check that the path is suitably absolute or relative:
                   (define dir
                     (case (cc-info-path-mode cc)
                       [(relative abs-in-relative)
                        (or (and (list? p)
                                 (info-relative->path p))
                            (and (complete-path? p)
                                 ;; `c' must be `(lib ...)'
                                 (list? c)
                                 (pair? c)
                                 (eq? 'lib (car c))
                                 (pair? (cdr c))
                                 (andmap string? (cdr c))
                                 ;; Path must match collection resolution:
                                 (with-handlers ([exn:fail? (lambda (exn) #f)])
                                   (equal? p (apply collection-path (cdr c))))
                                 p))]
                       [(abs)
                        (and (complete-path? p) p)]))
                   (if (and dir
                            (let ([omit-root
                                   (if (path? p)
                                       ;; absolute path => need a root for checking omits;
                                       ;; for a collection path of length N, go up N-1 dirs:
                                       (simplify-path (apply build-path p (for/list ([i (cddr c)]) 'up)) #f)
                                       ;; relative path => no root needed for checking omits:
                                       #f)])
                              (and (directory-exists? dir)
                                   (not (eq? 'all (omitted-paths dir getinfo omit-root)))))
                            (or (file-exists? (build-path dir "info.rkt"))
                                (file-exists? (build-path dir "info.ss"))))
                     (hash-set! t a (list b c d e))
                     (begin (when (verbose) (printf " drop entry: ~s\n" i))
                            (set! all-ok? #f)))]
                  [_ (when (verbose) (printf " bad entry: ~s\n" i))
                     (set! all-ok? #f)])))
            ;; Record the table loaded for this collection root in the
            ;; all-roots table:
            (hash-set! ht (cc-info-path cc) t)
            ;; If anything in the "cache.rktd" file was bad, then claim
            ;; that the old table was empty, so that we definitely write
            ;; the new table.
            (hash-set! ht-orig (cc-info-path cc)
                       (and all-ok? (hash-copy t)))
            t)))
      ;; Add this collection's info to the table, replacing any information
      ;; already there, if the collection has an "info.ss" file:
      (when (or (file-exists? (build-path (cc-path cc) "info.rkt"))
                (file-exists? (build-path (cc-path cc) "info.ss")))
        (hash-set! t
                   (if (eq? (cc-info-path-mode cc) 'relative)
                       ;; Use relative path:
                       (path->info-relative (apply build-path
                                                   (cc-info-root cc)
                                                   (cc-collection cc)))
                       ;; Use absolute path:
                       (path->bytes (cc-path cc)))
                   (cons (domain) (cc-shadowing-policy cc)))))
    ;; Write out each collection-root-specific table to a "cache.rktd" file:
    (hash-for-each ht
      (lambda (info-path ht)
        (unless (equal? ht (hash-ref ht-orig info-path))
          (define-values [base name dir?] (split-path info-path))
          (make-directory* base)
          (define p info-path)
          (setup-printf "updating" "~a" (path->relative-string/setup p))
          (when (verbose)
            (define ht0 (hash-ref ht-orig info-path))
            (when ht0
              (for ([(k v) (in-hash ht)])
                (define v2 (hash-ref ht0 k #f))
                (unless (equal? v v2)
                  (printf " ~s -> ~s\n   instead of ~s\n" k v v2)))
              (for ([(k v) (in-hash ht0)])
                (unless (hash-ref ht k #f)
                  (printf " ~s removed\n" k)))))
          (with-handlers ([exn:fail? (warning-handler (void))])
            (with-output-to-file p #:exists 'truncate/replace
              (lambda ()
                (write (hash-map ht cons))
                (newline))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                       Docs                    ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (scr:call name . xs)
    (parameterize ([current-namespace
                    (namespace-anchor->empty-namespace anchor)])
      (apply (dynamic-require 'setup/scribble name) xs)))

  (define (set-doc:verbose)
    (scr:call 'verbose (verbose)))

  (define (doc:setup-scribblings latex-dest auto-start-doc?)
    (scr:call 'setup-scribblings
              (parallel-workers)
              name-str
              (if no-specific-collections? #f (map cc-path ccs-to-compile))
              latex-dest auto-start-doc? (make-user)
              (lambda (what go alt) (record-error what "Building docs" go alt))
              setup-printf))

  (define (make-docs-step)
    (setup-printf #f "--- building documentation ---")
    (set-doc:verbose)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (setup-printf #f "docs failure: ~a" (exn->string exn)))])
      (doc:setup-scribblings #f (and (not (null? (archives)))
                                     (archive-implies-reindex)))))

  (define (doc-pdf-dest-step)
    (setup-printf #f "--- building PDF documentation (via pdflatex) ---")
    (define dest-dir (path->complete-path (doc-pdf-dest)))
    (unless (directory-exists? dest-dir)
      (make-directory dest-dir))
    (define tmp-dir
      (build-path (find-system-path 'temp-dir)
                  (format "pltpdfdoc~a" (current-seconds))))
    (dynamic-wind
      void
      (lambda ()
        (make-directory tmp-dir)
        (set-doc:verbose)
        (doc:setup-scribblings tmp-dir #f)
        (parameterize ([current-directory tmp-dir])
          (for ([f (directory-list)]
                #:when (regexp-match? #rx#"[.]tex$" (path-element->bytes f)))
            (define pdf (scr:call 'run-pdflatex f
                                  (lambda (fmt . xs)
                                    (apply setup-printf #f fmt xs))))
            (define target (build-path dest-dir pdf))
            (when (file-exists? target) (delete-file target))
            (copy-file pdf target))))
      (lambda ()
        (when (directory-exists? tmp-dir)
          (delete-directory/files tmp-dir)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make Launchers               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-launchers-step)
    (setup-printf #f "--- creating launchers ---")
    (define (name-list l)
      (unless (list-of relative-path-string? l)
        (error "result is not a list of relative path strings:" l)))
    (define (flags-list l)
      (unless (list-of (list-of string?) l)
        (error "result is not a list of strings:" l)))
    (define ((or-f f) x) (when x (f x)))
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
                "WARNING"
                "~s launcher name list ~s has no matching library/flags lists"
                kind mzlns))]
            [(and (or (not mzlls) (= (length mzlns) (length mzlls)))
                  (or (not mzlfs) (= (length mzlns) (length mzlfs))))
             (for ([mzln (in-list mzlns)]
                   [mzll (in-list (or mzlls (map (lambda (_) #f) mzlns)))]
                   [mzlf (in-list (or mzlfs (map (lambda (_) #f) mzlns)))])
               (define p (program-launcher-path mzln))
               (define aux
                 `((exe-name . ,mzln)
                   (framework-root . #f)
                   (dll-dir . #f)
                   (relative? . ,(not absolute-installation?))
                   ,@(build-aux-from-path
                      (build-path (cc-path cc)
                                  (path-replace-suffix (or mzll mzln) #"")))))
               (unless (up-to-date? p aux)
                 (setup-printf
                  "launcher"
                  "~a~a"
                  (case kind
                    [(gui)     (path->relative-string/gui-bin p)]
                    [(console) (path->relative-string/console-bin p)]
                    [else (error 'make-launcher "internal error (~s)" kind)])
                  (let ([v (current-launcher-variant)])
                    (if (eq? v (system-type 'gc)) "" (format " [~a]" v))))
                 (make-launcher
                  (or mzlf
                      (if (cc-collection cc)
                          (list "-l-" (string-append
                                       (string-append*
                                        (map (lambda (s) (format "~a/" s))
                                             (cc-collection cc)))
                                       mzll))
                          (list "-t-" (path->string (build-path (cc-path cc) mzll)))))
                  p
                  aux)))]
            [else
             (define fault
               (if (or (not mzlls) (= (length mzlns) (length mzlls))) 'f 'l))
             (setup-printf
              "WARNING"
              "~s launcher name list ~s doesn't match ~a list; ~s"
              kind mzlns
              (if (eq? 'l fault) "library" "flags")
              (if (eq? fault 'l) mzlls mzlfs))]))
        (for ([variant (available-gracket-variants)])
          (parameterize ([current-launcher-variant variant])
            (make-launcher 'gui
                           'gracket-launcher-names
                           'gracket-launcher-libraries
                           'gracket-launcher-flags
                           gracket-program-launcher-path
                           make-gracket-launcher
                           gracket-launcher-up-to-date?)
            (make-launcher 'gui
                           'mred-launcher-names
                           'mred-launcher-libraries
                           'mred-launcher-flags
                           mred-program-launcher-path
                           make-mred-launcher
                           mred-launcher-up-to-date?)))
        (for ([variant (available-racket-variants)])
          (parameterize ([current-launcher-variant variant])
            (make-launcher 'console
                           'racket-launcher-names
                           'racket-launcher-libraries
                           'racket-launcher-flags
                           racket-program-launcher-path
                           make-racket-launcher
                           racket-launcher-up-to-date?)
            (make-launcher 'console
                           'mzscheme-launcher-names
                           'mzscheme-launcher-libraries
                           'mzscheme-launcher-flags
                           mzscheme-program-launcher-path
                           make-mzscheme-launcher
                           mzscheme-launcher-up-to-date?))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; setup-unit Body                ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setup-printf "version" "~a [~a]" (version) (system-type 'gc))
  (setup-printf "variants" "~a" (string-join (map symbol->string (available-mzscheme-variants)) ", "))
  (setup-printf "main collects" "~a" (path->string main-collects-dir))
  (setup-printf "collects paths" (if (null? (current-library-collection-paths)) " empty!" ""))
  (for ([p (current-library-collection-paths)])
    (setup-printf #f "  ~a" (path->string p)))

  (when (and (not (null? (archives))) no-specific-collections?)
    (done))

  (when (clean) (clean-step))
  (when (make-zo)
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (do-install-part 'pre)

  (when (make-zo) (make-zo-step))
  (when (make-info-domain) (make-info-domain-step))

  (when (make-launchers) (make-launchers-step))

  (when (make-docs)
    ;; Double-check that "setup/scribble" is present.
    (when (file-exists? (collection-file-path "scribble.rkt" "setup"))
      (make-docs-step)))
  (when (doc-pdf-dest) (doc-pdf-dest-step))

  (do-install-part 'general)
  (do-install-part 'post)

  (done))
