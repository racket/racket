;; Expects parameters to be set before invocation.
;; Calls `exit' when done.

#lang racket/base

(require racket/path
         racket/file
         racket/port
         racket/match
         racket/system
         racket/list
         racket/string
         compiler/cm
         compiler/compilation-path
         planet/planet-archives
         planet/private/planet-shared
         (only-in planet/resolver resolve-planet-path)

         "option.rkt"
         compiler/compiler
         (prefix-in compiler:option: compiler/option)
         launcher/launcher
         dynext/file

         "unpack.rkt"
         "getinfo.rkt"
         "dirs.rkt"
         "matching-platform.rkt"
         "main-collects.rkt"
         "path-to-relative.rkt"
         "path-relativize.rkt"
         "private/omitted-paths.rkt"
         "parallel-build.rkt"
         "private/cc-struct.rkt"
         "link.rkt"
         "private/dylib.rkt"
         "private/elf.rkt"
         "private/pkg-deps.rkt"
         "collection-name.rkt"
         "private/format-error.rkt"
         compiler/private/dep
         (only-in pkg/lib pkg-directory
                  pkg-single-collection))

(define-namespace-anchor anchor)

;; Although we use `#:bootstrap?' mode for reading an "info.rkt" file,
;; which disables the use of compiled bytecode, also use whatever
;; namespace, .zo-use, and compilation configuration was in place for
;; loading setup (just in case), instead of whatever is in place for
;; the collections that setup is processing:
(define make-getinfo
  (let ([ns (namespace-anchor->empty-namespace anchor)]
        [compile (current-compile)]
        [loader (current-load/use-compiled)]
        [paths (use-compiled-file-paths)])
    (lambda (info-ns)
      (lambda (path)
        (parameterize ([current-namespace ns]
                       [current-compile compile]
                       [current-load/use-compiled loader]
                       [use-compiled-file-paths paths])
          (get-info/full path 
                         #:namespace info-ns
                         #:bootstrap? #t))))))

(provide setup-core)

(define (setup-core)

  (define name-str (setup-program-name))
  (define name-sym (string->symbol name-str))
  (define main-collects-dir (simple-form-path (find-collects-dir)))
  (define main-collects-dirs (for/hash ([p (in-list (get-main-collects-search-dirs))])
                               (values (simple-form-path p) #t)))
  (define main-links-files (for/hash ([p (in-list (get-links-search-files))])
                             (values (simple-form-path p) #t)))
  (define mode-dir
    (if (compile-mode)
      (build-path "compiled" (compile-mode))
      (build-path "compiled")))

  (unless (make-user)
    (current-library-collection-paths
     (for/list ([p (current-library-collection-paths)]
                #:when (hash-ref main-collects-dirs p #f))
       p)))

  (current-library-collection-paths
   (if (member #f (current-library-collection-links))
       ;; Normal case, include current library collection paths:
       (map simple-form-path (current-library-collection-paths))
       ;; No `#f' in links list means that we don't look at
       ;; the current library collection paths:
       null))

  (define (setup-fprintf p task s . args)
    (let ([task (if task (string-append task ": ") "")])
      (apply fprintf p (string-append name-str ": " task s "\n") args)
      (flush-output p)))

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

  (define path->relative-string/lib
    (make-path->relative-string
     (list (cons find-lib-dir "<lib>/"))))

  (define path->relative-string/share
    (make-path->relative-string
     (list (cons find-share-dir "<share>/"))))

  (define path->relative-string/man
    (make-path->relative-string
     (list (cons find-man-dir "<man>/"))))

  (define-values (path->main-lib-relative
                  main-lib-relative->path)
    (make-relativize find-lib-dir
                     'lib
                     'path->main-lib-relative
                     'main-lib-relative->path))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   Errors                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errors null)
  (define exit-code 0)
  (define original-thread (current-thread))
  (define (append-error cc desc exn out err type)
    (set! errors (cons (list cc desc exn out err type) errors))
    (when (fail-fast)
      (break-thread original-thread)))
  (define (handle-error cc desc exn out err type)
    (define long? #t) ; possibly better: (define long? (verbose))
    (cond
     [(exn? exn)
      (format-error exn #:long? long?)]
     [(and (pair? exn) (string? (car exn)) (string? (cdr exn)))
      (eprintf "~a\n" ((if long? car cdr) exn))])
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
        (setup-fprintf port type "during ~a for ~a" desc (cond
                                                          [(cc? cc) (cc-name cc)]
                                                          [(path? cc)
                                                           (path->relative-string/setup cc #:cache pkg-path-cache)]
                                                          [else cc]))
        (let ([msg (cond
                    [(exn? x)
                     (format-error x #:long? #f #:to-string? #t #:cache pkg-path-cache)]
                    [(not x)
                     ;; No error; just output
                     #f]
                    [else
                     ;; `x` is a pair of strings, long and short forms of the error:
                     (cdr x)])])
          (when x
            (for ([str (in-list (regexp-split #rx"\n" msg))])
              (setup-fprintf port #f "  ~a" str))))
        (unless (zero? (string-length out)) (eprintf "STDOUT:\n~a=====\n" out))
        (unless (zero? (string-length err)) (eprintf "STDERR:\n~a=====\n" err)))))

  (define (done)
    (unless (null? errors)
      (setup-printf #f "--- summary of errors ---")
      (show-errors (current-error-port))
      (when (pause-on-errors)
        (eprintf "INSTALLATION FAILED.\nPress Enter to continue...\n")
        (read-line))
      (exit 1))
    (exit exit-code))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Archive Unpacking               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define make-docs?
    (and (make-docs)
         ;; Double-check that `setup/scribble' is present:
         (let ([p (collection-file-path "scribble.rkt" "setup")])
           (or (file-exists? p)
               (file-exists? (get-compilation-bytecode-file p))))))

  (define (pkg->collections pkg)
    (define dir (pkg-directory pkg))
    (cond
     [dir
      (define collect (pkg-single-collection dir #:name pkg))
      (if collect
          (list (list collect))
          (for/list ([d (directory-list dir)]
                     #:when (and (directory-exists? (build-path dir d))
                                 (collection-name-element? (path->string d))))
            (list d)))]
     [else
      (error 'pkd->collections
             (string-append "package not found\n"
                            "  package: ~a")
             pkg)]))
    
  (define x-specific-collections
    (append* (specific-collections)
             (apply append
                    (map pkg->collections
                         (specific-packages)))
             (if (and (make-doc-index)
                      make-docs?)
                 (append
                  (if (not (avoid-main-installation))
                      '(("scribblings/main"))
                      null)
                  (if (make-user)
                      '(("scribblings/main/user"))
                      null))
                 null)
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
    (and (null? x-specific-collections)
         (null? x-specific-planet-dirs)
         (not (make-only))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;              Find Collections                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define pkg-path-cache (make-hash))

  (define info-ns (make-base-namespace))
  (define getinfo (make-getinfo info-ns))

  (define (make-cc* collection parent path omit-root info-root 
                    info-path info-path-mode shadowing-policy 
                    main?)
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
    (define path-name (path->relative-string/setup path #:cache pkg-path-cache))
    (when (info 'compile-subcollections (lambda () #f))
      (setup-printf "WARNING"
                    "ignoring `compile-subcollections' entry in info ~a"
                    path-name))
    ;; this check is also done in compiler/compiler, in compile-directory
    (and (not (eq? 'all (omitted-paths path getinfo omit-root)))
         (make-cc collection path
                  (if name
                      (format "~a (~a)" path-name name)
                      path-name)
                  info
                  parent
                  omit-root
                  info-root info-path info-path-mode
                  shadowing-policy
                  main?)))

  (define ((warning-handler v) exn)
    (setup-printf "WARNING" "~a" (exn->string exn))
    v)

  ;; Maps a colletion name to a list of `cc's:
  (define collection-ccs-table (make-hash))

  ;; collection-cc! : listof-path .... -> cc
  (define (collection-cc! collection-p
                          #:parent [parent-cc #f]
                          #:path [dir (apply collection-path collection-p)]
                          #:omit-root [omit-root #f]
                          #:info-root [info-root #f]
                          #:info-path [info-path #f]
                          #:info-path-mode [info-path-mode 'relative]
                          #:main? [main? #f])
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
                parent-cc
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
                (list (cons 'lib (map path->string collection-p)) 1 0)
                main?))
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
                   #f
                   path
                   omit-root
                   #f ; don't need info-root; absolute paths in cache.rktd will be ok
                   (get-planet-cache-path)
                   'abs
                   (list `(planet ,owner ,pkg-file ,@extra-path) maj min)
                   #f)))

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

  (define (skip-collection-directory? collection)
    ;; Skiping ".git" or ".svn" makes it cleaner to use a git of subversion
    ;; checkout as a collection directory
    (regexp-match? #rx"[.](git|svn)$" (path->bytes collection)))

  ;; Add in all non-planet collections, first from 
  ;; `current-library-collection-paths':
  (for ([cp (current-library-collection-paths)]
        #:when (directory-exists? cp)
        [collection (directory-list cp)]
        #:unless (skip-collection-directory? collection)
        #:when (directory-exists? (build-path cp collection)))
    (collection-cc! (list collection)
                    #:info-root cp
                    #:path (build-path cp collection)
                    #:main? (hash-ref main-collects-dirs cp #f)))
  ;; Now from `current-library-collection-links' for installation-wide
  ;; links:
  (let ()
    (define info-root (find-share-dir))
    (define info-path (build-path info-root "info-cache.rktd"))
    (define (cc! col #:path path)
      (collection-cc! col
                      #:path path
                      #:info-root info-root
                      #:info-path info-path
                      #:info-path-mode 'abs-in-relative
                      #:omit-root 'dir
                      #:main? #t))
    (for ([inst-links (in-list (current-library-collection-links))]
          #:when (and (path? inst-links)
                      (hash-ref main-links-files (simple-form-path inst-links) #f)))
      (for ([c+p (in-list (links #:file inst-links #:with-path? #t))])
        (cc! (list (string->path (car c+p)))
             #:path (cdr c+p)))
      (for ([cp (in-list (links #:root? #t #:file inst-links))]
            #:when (directory-exists? cp)
            [collection (directory-list cp)]
            #:unless (skip-collection-directory? collection)
            #:when (directory-exists? (build-path cp collection)))
        (cc! (list collection)
             #:path (build-path cp collection)))))
  ;; Now from `current-library-collection-links' for user-specific
  ;; links:
  (when (make-user)
    (define info-root (find-user-share-dir))
    (define info-path (build-path info-root "info-cache.rktd"))
    (define (cc! col #:path path)
      (collection-cc! col
                      #:path path
                      #:info-root info-root
                      #:info-path info-path
                      #:info-path-mode 'abs-in-relative
                      #:omit-root 'dir))
    ;; A links spec in `current-library-collection-links' counts as
    ;; user-specific when it's not in `make-links-files':
    (for ([inst-links (in-list (current-library-collection-links))]
          #:unless (and (path? inst-links)
                        (hash-ref main-links-files (simple-form-path inst-links) #f)))
      (cond
       [(not inst-links) ; covered by `current-library-collection-paths'
        (void)]
       [(path? inst-links)
        (for ([c+p (in-list (links #:file inst-links #:with-path? #t))])
          (cc! (list (string->path (car c+p)))
               #:path (cdr c+p)))
        (for ([cp (in-list (links #:file inst-links #:root? #t))]
              #:when (directory-exists? cp)
              [collection (directory-list cp)]
              #:unless (skip-collection-directory? collection)
              #:when (directory-exists? (build-path cp collection)))
          (cc! (list collection) #:path (build-path cp collection)))]
       [else ; must be a hash table that simulates a links file:
        (for ([(coll-sym dir) (in-hash inst-links)])
          (cond
           [coll-sym
            ;; A single collection
            (cc! (string-split "/" (symbol->string coll-sym)) #:path dir)]
           [(directory-exists? dir)
            ;; A directory that holds collections:
            (for ([collection (directory-list dir)]
                  #:unless (skip-collection-directory? collection)
                  #:when (directory-exists? (build-path dir collection)))
              (cc! (list collection) #:path (build-path dir collection)))]))])))

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

  (define (collection-tree-map collections-to-compile)
    (define (build-collection-tree cc)
      (define (make-child-cc parent-cc name)
        (collection-cc! (append (cc-collection parent-cc) (list name))
                        #:parent parent-cc
                        #:path (build-path (cc-path parent-cc) name)
                        #:info-root (cc-info-root cc)
                        #:info-path (cc-info-path cc)
                        #:info-path-mode (cc-info-path-mode cc)
                        #:omit-root (cc-omit-root cc)
                        #:main? (cc-main? cc)))
      (define info (cc-info cc))
      (define ccp  (cc-path cc))
      ;; note: omit can be 'all, if this happens then this collection
      ;; should not have been included, but we might jump in if a
      ;; command-line argument specified a coll/subcoll
      (define omit (append
                    (if make-docs?
                        null
                        (list (string->path "scribblings")))
                    (omitted-paths ccp getinfo (cc-omit-root cc))))
      (define-values [dirs files]
        (if (eq? 'all omit)
            (values null null)
            (partition (lambda (x) (directory-exists? (build-path ccp x)))
                       (filter (lambda (p) (not (member p omit)))
                               (directory-list ccp)))))
      (define children-ccs
        (map build-collection-tree
             (filter-map (lambda (x) (make-child-cc cc x)) dirs)))
      (define srcs
        (append
         (filter extract-base-filename/ss files)
         (if make-docs?
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
                             #:parent cc
                             #:path (build-path (cc-path cc) child)
                             #:info-root (cc-info-root cc)
                             #:info-path (cc-info-path cc)
                             #:info-path-mode (cc-info-path-mode cc)
                             #:omit-root (cc-omit-root cc)
                             #:main? (cc-main? cc)))
           children))
    (collection-closure collections-to-compile make-children-ccs))

  (define (lookup-collection-closure collections-to-compile)
    (define ht (make-hash))
    (for ([cc (in-list collections-to-compile)])
      (hash-set! ht cc #t))
    (define (lookup-children-ccs! cc children)
      (apply
       append
       (for/list ([child (in-list children)])
         (for/list ([cc (in-list (collection->ccs (append (cc-collection cc) (list child))))]
                    #:unless (hash-ref ht cc #f))
           (hash-set! ht cc #t)
           cc))))
    (collection-closure collections-to-compile lookup-children-ccs!)
    (for/list ([v (in-hash-keys ht)]) v))

  (define all-collections-closure (plt-collection-closure all-collections))

  (define (check-against-all given-ccs nothing-else-to-do?)
    (when (and (null? given-ccs)
               nothing-else-to-do?
               (not (make-tidy)))
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
    ((if (avoid-main-installation)
         (lambda (l) (filter (lambda (cc) (not (cc-main? cc))) l))
         values)
     (if no-specific-collections?
         all-collections
         (check-against-all
          (append-map
           (lambda (c)
             (define sc (map (lambda (s) (if (path? s) (path->string s) s))
                             c))
             (define elems
               (append-map (lambda (s) (map string->path (regexp-split #rx"/" s)))
                           sc))
             (define ccs (collection->ccs elems))
             (when (null? ccs)
               ;; let `collection-path' complain about the name, if that's the problem:
               (with-handlers ([exn? (compose1 raise-user-error exn-message)])
                 (apply collection-path elems))
               ;; otherwise, it's probably a collection with nothing to compile
               ;; spell the name
               (setup-printf "WARNING"
                             "nothing to compile in a given collection path: \"~a\""
                             (string-join sc "/")))
             ccs)
           x-specific-collections)
          (null? planet-collects)))))

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
          (unless (external-dep? s)
              (define new-s (dep->path s))
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

  (define (assume-virtual-sources? cc)
    (or ((cc-info cc) 'assume-virtual-sources (lambda () #f))
        (let ([cc (cc-parent-cc cc)])
          (and cc
               (assume-virtual-sources? cc)))))

  (define (clean-collection cc dependencies)
    (begin-record-error cc "cleaning"
      (define info (cc-info cc))
      (define paths
        (call-info
         info
         'clean
         (lambda ()
           (if (assume-virtual-sources? cc)
               null
               (list mode-dir
                     (build-path mode-dir "native")
                     (build-path mode-dir "native" (system-library-subpath)))))
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
                        (path->relative-string/setup (cc-path cc) 
                                                     #:cache pkg-path-cache))))
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
             (setup-printf "deleting" "~a" (path->relative-string/setup zo #:cache pkg-path-cache))
             (delete-file/record-dependency zo dependencies)
             (delete-file/record-dependency dep dependencies))))
        (when did-something? (loop dependencies)))
      (setup-printf #f "clearing info-domain caches")
      (define (check-one-info-domain fn)
        (when (file-exists? fn)
          (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
            (with-output-to-file fn void #:exists 'truncate/replace))))
      (for ([p (current-library-collection-paths)])
        (check-one-info-domain (build-path p "info-domain" "compiled" "cache.rktd")))
      (check-one-info-domain (build-path (find-share-dir) "info-cache.rktd"))
      (check-one-info-domain (build-path (find-user-share-dir) "info-cache.rktd"))
      (setup-printf #f "deleting documentation databases")
      (for ([d (in-list (list (find-doc-dir) (find-user-doc-dir)))])
        (when d
          (define f (build-path d "docindex.sqlite"))
          (when (file-exists? f)
            (delete-file f))))))

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
                                   [(pre)     "early install"]
                                   [(general) "general install"]
                                   [(post)    "post install"])
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
                  (unless (or (file-exists? p)
                              (bytecode-file-exists? p))
                    (error "installer file does not exista: " p)))))
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
            (cond
             [(procedure-arity-includes? installer 3)
              (installer dir (cc-path cc) (not (cc-main? cc)))]
             [(procedure-arity-includes? installer 2)
              (installer dir (cc-path cc))]
             [else
              (installer dir)]))))))

  (define (bytecode-file-exists? p)
    (parameterize ([use-compiled-file-paths (list mode-dir)])
      (define zo (get-compilation-bytecode-file	p))
      (file-exists? zo)))

  (define (this-platform? info)
    (define sys
      (call-info info
                 'install-platform
                 (lambda () #rx"")
                 (lambda (v)
                   (unless (or (regexp? v)
                               (string? v)
                               (symbol? v))
                     (error "entry is not regexp, string, or symbol:" v)))))
    (matching-platform? sys))

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

  (define (clean-cc cc dir info)
    ;; Clean up bad .zos:
    (unless (assume-virtual-sources? cc)
      (define roots
        ;; If there's more than one relative root, then there will
        ;; be multiple ways to get to a ".zo" file, and our strategy
        ;; below will fail. Give up on checking relative roots in
        ;; that case.
        (let ([roots (current-compiled-file-roots)])
          (if (1 . < . (for/sum ([r (in-list roots)])
                         (if (or (eq? r 'same)
                                 (relative-path? r))
                             1
                             0)))
              ;; give up on relative:
              (filter (lambda (p) (and (path? p) (absolute-path? p)))
                      roots)
              ;; all roots ok:
              roots)))
      ;; Try each compile-file root, but preserve the list of allowed
      ;; bytecode files after it's computed the first time.
      (for/fold ([ok-zo-files #f]) ([root (in-list roots)])
        (define c (cond
                   [(eq? root 'same) (build-path dir mode-dir)]
                   [(relative-path? root)
                    (build-path dir root mode-dir)]
                   [else
                    (reroot-path (build-path dir mode-dir) root)]))
        (cond
         [(directory-exists? c)
          ;; Directory for compiled files exist...
          (let ([ok-zo-files
                 (or ok-zo-files
                     ;; Build table of allowed ".zo" file names that can
                     ;; appear in a "compiled" directory:
                     (make-immutable-hash
                      (map (lambda (p)
                             (cons (path-add-suffix p #".zo") #t))
                           (append (directory-list dir)
                                   (info 'virtual-sources (lambda () null))))))])
            ;; Check each file in `c` to see whether it can stay:
            (for ([p (directory-list c)])
              (when (and (regexp-match #rx#".(zo|dep)$" (path-element->bytes p))
                         (not (hash-ref ok-zo-files (path-replace-suffix p #".zo") #f)))
                (setup-fprintf (current-error-port) #f " deleting ~a" (build-path c p))
                (delete-file (build-path c p))))
            ok-zo-files)]
         [else ok-zo-files]))))

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
                            (path->complete-path where (cc-path cc))
                            #:cache pkg-path-cache)))
         (lambda ()
           (define dir  (cc-path cc))
           (define info (cc-info cc))
           (clean-cc cc dir info)
           (compile-directory-zos dir info
                                  #:omit-root (cc-omit-root cc)
                                  #:managed-compile-zo caching-managed-compile-zo
                                  #:skip-path (and (avoid-main-installation) main-collects-dir)
                                  #:skip-doc-sources? (not make-docs?))))))
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
      (partition (lambda (x) (not (regexp-match? name (cc-name (car x))))) cct))
    (define (move-to where names cct)
      (for/fold ([cct cct]) ([name (in-list (reverse names))])
        (define-values [diff same] (partition-cct name cct))
        (case where
          [(beginning) (append same diff)]
          [(end) (append diff same)])))
    (setup-printf #f "--- compiling collections ---")
    (if ((parallel-workers) . > . 1)
      (begin
        (when (or no-specific-collections?
                  (member "racket" x-specific-collections))
          (for/fold ([gcs 0]) ([cc (in-list (collection->ccs (list (string->path "racket"))))])
            (when (and (cc-main? cc)
                       (member (cc-info-root cc)
                               (current-library-collection-paths)))
              (compile-cc cc 0))))
        (with-specified-mode
          (lambda ()
            (define cct
              (move-to 'beginning (list #rx"/compiler$" #rx"/raco$" #rx"/racket$" #rx"<pkgs>/images/")
                       (move-to 'end (list #rx"<pkgs>/drracket")
                                (sort-collections-tree
                                 (collection-tree-map top-level-plt-collects)))))
            (iterate-cct (lambda (cc)
                           (define dir (cc-path cc))
                           (define info (cc-info cc))
                           (clean-cc cc dir info))
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
    (define (get-info-ht info-root info-path info-path-mode)
      (define-values (path->info-relative info-relative->path)
        (apply values
               (hash-ref roots
                         info-root
                         (lambda ()
                           (define-values [p-> ->p]
                             (if info-root
                                 (make-relativize (lambda () info-root)
                                                  'info
                                                  'path->info-relative
                                                  'info-relative->path)
                                 (values #f #f)))
                           (hash-set! roots info-root (list p-> ->p))
                           (list p-> ->p)))))
      (hash-ref ht info-path
                (lambda ()
                  ;; No table for this root, yet. Build one.
                  (define l
                    (let ([p info-path])
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
                        [(list (and a (or (? bytes?) (list (or 'info 'lib) (? bytes?) ...)))
                               (list (? symbol? b) ...) c (? integer? d) (? integer? e))
                         (define p (if (bytes? a) (bytes->path a) a))
                         ;; Check that the path is suitably absolute or relative:
                         (define dir
                           (case info-path-mode
                             [(relative abs-in-relative)
                              (or (and (list? p)
                                       (if (eq? (car p) 'info)
                                           (info-relative->path p)
                                           (main-lib-relative->path p)))
                                   ;; `c' must be `(lib ...)'
                                  (and (or (relative-path? p)
                                           ;; Keep a complete path only if it could not be
                                           ;; made relative:
                                           (and (complete-path? p)
                                                (complete-path?
                                                 (find-relative-path info-root
                                                                     p
                                                                     #:more-than-root? #t))))
                                       (list? c)
                                       (pair? c)
                                       (eq? 'lib (car c))
                                       (pair? (cdr c))
                                       (andmap string? (cdr c))
                                       (let ([p (simplify-path (path->complete-path p info-root))])
                                         (and
                                          ;; path must match some cc:
                                          (for/or ([cc (in-list all-collections-closure)])
                                            (equal? p (cc-path cc)))
                                          p))))]
                             [(abs)
                              (and (complete-path? p)
                                   (match c
                                     [(list 'planet (? string? a) (? string? pk))
                                      ;; Check that the package is installed and maps to `p`:
                                      (and (get-installed-package a pk d e)
                                           (let ([bp (resolve-planet-path
                                                      `(planet "bogus.rkt" (,a ,pk ,d ,e)))])
                                             (and (path? bp)
                                                  (let-values ([(base name dir?) (split-path bp)])
                                                    (and (path? base)
                                                         (equal? (path->directory-path p)
                                                                 (path->directory-path base)))))))]
                                     [else
                                      #t])
                                   p)]))
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
                  (hash-set! ht info-path t)
                  ;; If anything in the "cache.rktd" file was bad, then claim
                  ;; that the old table was empty, so that we definitely write
                  ;; the new table.
                  (hash-set! ht-orig info-path
                             (and all-ok? (hash-copy t)))
                  t)))
    ;; process all collections:
    (for ([cc ccs-to-compile])
      (define domain
        (with-handlers ([exn:fail? (lambda (x) (lambda () null))])
          (parameterize ([current-namespace info-ns])
            (dynamic-require (build-path (cc-path cc) "info.rkt")
                             '#%info-domain))))
      ;; Get the table for this cc's info-domain cache:
      (define t (get-info-ht (cc-info-root cc)
                             (cc-info-path cc)
                             (cc-info-path-mode cc)))
      (define-values (path->info-relative info-relative->path)
        ;; Look up value that was forced by by `get-info-ht':
        (apply values (hash-ref roots (cc-info-root cc))))
      ;; Add this collection's info to the table, replacing any information
      ;; already there, if the collection has an "info.ss" file:
      (when (or (file-exists? (build-path (cc-path cc) "info.rkt"))
                (file-exists? (build-path (cc-path cc) "info.ss")))
        (hash-set! t
                   (case (cc-info-path-mode cc)
                     [(relative)
                       ;; Use relative path:
                      (path->info-relative (apply build-path
                                                  (cc-info-root cc)
                                                  (cc-collection cc)))]
                     [(abs-in-relative) 
                      ;; Try relative to `lib':
                      (let ([p (path->main-lib-relative (cc-path cc))])
                        (if (path? p)
                            ;; Fall back to relative (with ".."s) to info root:
                            (path->bytes (find-relative-path (cc-info-root cc)
                                                             p
                                                             #:more-than-root? #t))
                            p))]
                     [else (path->bytes (cc-path cc))])
                   (cons (domain) (cc-shadowing-policy cc)))))
    ;; In "tidy" mode, make sure we check each "cache.rktd":
    (when (or (make-tidy)
              no-specific-collections?)
      (for ([c (in-list (current-library-collection-paths))])
        (when (and (directory-exists? c)
                   (not (and (avoid-main-installation)
                             (hash-ref main-collects-dirs c #f))))
          (define info-path (build-path c "info-domain" "compiled" "cache.rktd"))
          (when (file-exists? info-path)
            (get-info-ht c info-path 'relative))))
      (unless (avoid-main-installation)
        (define info-root (find-share-dir))
        (define info-path (build-path info-root "info-cache.rktd"))
        (when (file-exists? info-path)
          (get-info-ht info-root info-path 'abs-in-relative)))
      (when (make-user)
        (define info-root (find-user-share-dir))
        (define info-path (build-path info-root "info-cache.rktd"))
        (when (file-exists? info-path)
          (get-info-ht info-root info-path 'abs-in-relative))
        (define planet-info-path (get-planet-cache-path))
        (when (file-exists? planet-info-path)
          (get-info-ht #f planet-info-path 'abs))))
    ;; Write out each collection-root-specific table to a "cache.rktd" file:
    (hash-for-each ht
      (lambda (info-path ht)
        (unless (equal? ht (hash-ref ht-orig info-path))
          (define-values [base name dir?] (split-path info-path))
          (make-directory* base)
          (define p info-path)
          (setup-printf "updating" "~a" (path->relative-string/setup
                                         p
                                         #:cache pkg-path-cache))
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
                (newline)))))))
    ;; Flush cached state in the current namespace:
    (reset-relevant-directories-state!))

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
              latex-dest auto-start-doc? (make-user) (force-user-docs)
              (make-tidy) (avoid-main-installation)
              (lambda (what go alt) (record-error what "building docs" go alt))
              setup-printf))

  (define (make-docs-step)
    (setup-printf #f "--- building documentation ---")
    (set-doc:verbose)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (set! exit-code 1)
                       (setup-printf #f "docs failure: ~a" (exn->string exn)))])
      (define auto-start-doc?
        (or (and (not (null? (archives)))
                 (archive-implies-reindex))
            (make-doc-index)))
      (doc:setup-scribblings #f auto-start-doc?)))

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
    (define created-launchers (make-hash))
    (for ([cc ccs-to-compile])
      (begin-record-error cc "launcher setup"
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
               (define p (program-launcher-path mzln #:user? (not (cc-main? cc))))
               (define receipt-path
                 (build-path (if (cc-main? cc)
                                 (find-lib-dir)
                                 (find-user-lib-dir))
                             "launchers.rktd"))
               (define (prep-dir p)
                 (define dir (path-only p))
                 (make-directory* dir))
               (prep-dir p)
               (prep-dir receipt-path)
               (hash-set! created-launchers
                          (record-launcher receipt-path mzln kind (current-launcher-variant) 
                                           (cc-collection cc) (cc-path cc))
                          #t)
               (define aux
                 (append
                  `((exe-name . ,mzln)
                    (relative? . ,(and (cc-main? cc)
                                       (not (get-absolute-installation?))))
                    (install-mode . ,(if (cc-main? cc) 'main 'user))
                    ,@(build-aux-from-path
                       (build-path (cc-path cc)
                                   (path-replace-suffix (or mzll mzln) #""))))))
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
                           mzscheme-launcher-up-to-date?)))))
    (when (or no-specific-collections?
              (make-tidy))
      (unless (avoid-main-installation)
        (tidy-launchers #f
                        (find-console-bin-dir)
                        (find-gui-bin-dir)
                        (find-lib-dir)
                        created-launchers
                        ccs-to-compile))
      (when (make-user)
        (tidy-launchers #t
                        (find-user-console-bin-dir)
                        (find-user-gui-bin-dir)
                        (find-user-lib-dir)
                        created-launchers
                        ccs-to-compile))))

  (define (read-receipt-hash receipt-path)
    (if (file-exists? receipt-path)
        (with-handlers ([exn:fail?
                         (lambda (exn)
                           (setup-printf
                            "WARNING"
                            "error reading receipts ~s: ~a"
                            receipt-path
                            (exn-message exn))
                           #hash())])
          (call-with-input-file* 
           receipt-path 
           (lambda (i)
             (define ht (read i))
             (if (hash? ht)
                 ht
                 (error "content is not a hash table")))))
        #hash()))

  (define (write-receipt-hash receipt-path ht)
    (call-with-output-file*
     #:exists 'truncate/replace
     receipt-path
     (lambda (o) (write ht o) (newline o))))

  (define (record-launcher receipt-path name kind variant coll coll-path)
    (let ([ht (read-receipt-hash receipt-path)])
      (define exe-key (vector kind
                              variant
                              name))
      (define exe-val (map path->string coll))
      (unless (equal? (hash-ref ht exe-key #f)
                      exe-val)
        (let ([ht (hash-set ht exe-key exe-val)]) 
          (write-receipt-hash receipt-path ht)))
      exe-key))

  (define (tidy-launchers user? bin-dir gui-bin-dir lib-dir created ccs-to-compile)
    (define receipt-path (build-path lib-dir "launchers.rktd"))
    (define ht (read-receipt-hash receipt-path))
    (define ht2 (for/fold ([ht (hash)]) ([(k v) (in-hash ht)])
                  (define coll-path (and (pair? v)
                                         (list? v)
                                         (andmap path-string? v)
                                         (apply collection-path v #:fail (lambda (s) #f))))
                  (cond
                   [(hash-ref created k #f)
                    ;; just created it, so keep it
                    (hash-set ht k v)]
                   [(and coll-path
                         ;; If we set up this collection, then the launcher
                         ;; must be in the created list if it's to be kept:
                         (let ([coll (map string->path v)])
                           (not
                            (for/or ([cc (in-list ccs-to-compile)])
                              (equal? coll (cc-collection cc))))))
                    ;; keep the launcher
                    (hash-set ht k v)]
                   [else
                    ;; remove the launcher
                    (define kind (vector-ref k 0))
                    (define variant (vector-ref k 1))
                    (define name (vector-ref k 2))
                    (parameterize ([current-launcher-variant variant])
                      (define exe-path ((if (eq? kind 'gui)
                                            gracket-program-launcher-path
                                            racket-program-launcher-path)
                                        name
                                        #:user? user?))
                      (define is-dir?
                        (if (eq? kind 'gui)
                            (gracket-launcher-is-actually-directory?)
                            (racket-launcher-is-actually-directory?)))
                      (define rel-exe-path
                        ((if (eq? kind 'gui)
                             path->relative-string/gui-bin 
                             path->relative-string/console-bin)
                         exe-path))
                      (cond
                       [(and (not is-dir?) (file-exists? exe-path))
                        (setup-printf "deleting" "launcher ~a" rel-exe-path)
                        (delete-file exe-path)]
                       [(and is-dir? (directory-exists? exe-path))
                        (setup-printf "deleting" "launcher ~a" rel-exe-path)
                        (delete-directory/files exe-path)])
                      ;; Clean up any associated .desktop file and icon file:
                      (when (eq? 'unix (system-type))
                        (let ([desktop (installed-executable-path->desktop-path
                                        exe-path
                                        user?)])
                          (when (file-exists? desktop)
                            (setup-printf "deleting" "desktop file ~a" 
                                          (path->relative-string/share desktop))
                            (delete-file desktop))
                          (for ([ext (in-list '(#"ico" #"png"))])
                            (define icon (installed-desktop-path->icon-path desktop
                                                                            user?
                                                                            ext))
                            (when (file-exists? icon)
                              (setup-printf "deleting" "icon file ~a" 
                                            (path->relative-string/share icon))
                              (delete-file icon))))))
                    ht])))
    (unless (equal? ht ht2)
      (setup-printf "updating" "launcher list")
      (write-receipt-hash receipt-path ht2)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       Foriegn Libraries and Man Pages         ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-copy/move-step what
                               whats
                               what/title
                               copy-tag
                               move-tag
                               find-target-dir
                               find-user-target-dir
                               path->relative-string/*
                               receipt-file
                               receipt-at-dest?
                               check-entry
                               build-dest-path
                               this-platform?
                               fixup-lib
                               copy-user-lib)
    (define (make-libs-step)
      (setup-printf #f (format "--- installing ~a ---" whats))
      (define installed-libs (make-hash))
      (define dests (make-hash))
      (for ([cc ccs-to-compile])
        (begin-record-error cc what/title
                            (define info (cc-info cc))
                            (define copy-libs
                              (call-info info copy-tag (lambda () null) check-entry))
                            (define move-libs
                              (call-info info move-tag (lambda () null) check-entry))

                            (unless (or (and (null? copy-libs)
                                             (null? move-libs))
                                        (not (this-platform? info)))
                              (define dir (if (cc-main? cc)
                                              (find-target-dir)
                                              (find-user-target-dir)))
                              (define r-dir (if receipt-at-dest?
                                                dir
                                                (if (cc-main? cc)
                                                    (find-lib-dir)
                                                    (find-user-lib-dir))))
                              (define receipt-path (build-path r-dir receipt-file))
                              (make-directory* dir)
                              (make-directory* r-dir)

                              (define (copy-lib lib moving?)
                                (define src (path->complete-path lib (cc-path cc)))
                                (define lib-name (file-name-from-path lib))
                                (define dest (build-dest-path dir lib-name))
                                (define already? (or (and moving?
                                                          (not (file-exists? src))
                                                          (not (directory-exists? src))
                                                          (or (file-exists? dest)
                                                              (directory-exists? dest)))
                                                     (same-content? src dest)))
                                (unless already?
                                  (setup-printf "installing" (string-append what " ~a")
                                                (path->relative-string/* dest)))
                                (hash-set!
                                 installed-libs
                                 (record-lib receipt-path lib-name (cc-collection cc) (cc-path cc))
                                 #t)
                                (unless already?
                                  (hash-set! dests dest #t)
                                  (delete-directory/files/hard dest)
				  (make-parent-directory* dest)
                                  (if (file-exists? src)
                                      (if (cc-main? cc)
                                          (copy-file src dest)
                                          (copy-user-lib src dest))
                                      (copy-directory/files src dest)))
                                src)
                              
                              (for ([lib (in-list copy-libs)])
                                (copy-lib lib #f))
                              
                              (for ([lib (in-list move-libs)])
                                (define src (copy-lib lib #t))
                                (delete-directory/files src #:must-exist? #f)))))
      (when (or no-specific-collections?
                (make-tidy))
        (unless (avoid-main-installation)
          (tidy-libs #f
                     (find-target-dir)
                     (if receipt-at-dest?
                         (find-target-dir)
                         (find-lib-dir))
                     installed-libs
                     ccs-to-compile))
        (when (make-user)
          (tidy-libs #t
                     (find-user-target-dir)
                     (if receipt-at-dest?
                         (find-user-target-dir)
                         (find-user-lib-dir))
                     installed-libs
                     ccs-to-compile)))
      (for-each fixup-lib (hash-keys dests)))

    (define (same-content? a b)
      (cond
       [(file-exists? a)
        (cond
         [(file-exists? b)
          (call-with-input-file* 
           a
           (lambda (a)
             (call-with-input-file* 
              b
              (lambda (b)
                (define as (make-bytes 4096))
                (define bs (make-bytes 4096))
                (let loop ()
                  (define an (read-bytes! as a))
                  (define bn (read-bytes! bs b))
                  (and (equal? an bn)
                       (equal? as bs)
                       (or (eof-object? an)
                           (loop))))))))]
         [else #f])]
       [(directory-exists? a)
        (cond
         [(directory-exists? b)
          (define (path<? a b) (bytes<? (path->bytes a) (path->bytes b)))
          (define al (sort (directory-list a) path<?))
          (define bl (sort (directory-list b) path<?))
          (and (equal? al bl)
               (andmap same-content?
                       (map (lambda (f) (build-path a f)) al)
                       (map (lambda (f) (build-path b f)) bl)))]
         [else #f])]
       [else #f]))

    (define (record-lib receipt-path name coll coll-path)
      (let ([ht (read-receipt-hash receipt-path)])
        (define lib-key (path-element->bytes name))
        (define lib-val (map path->string coll))
        (unless (equal? (hash-ref ht lib-key #f)
                        lib-val)
          (let ([ht (hash-set ht lib-key lib-val)]) 
            (write-receipt-hash receipt-path ht)))
        lib-key))

    (define (tidy-libs user? target-dir receipt-dir installed-libs ccs-to-compile)
      (clean-previous-delete-failures receipt-dir path->relative-string/*)
      (define receipt-path (build-path receipt-dir receipt-file))
      (define ht (read-receipt-hash receipt-path))
      (define ht2 (for/fold ([ht (hash)]) ([(k v) (in-hash ht)])
                    (define coll-path (and (pair? v)
                                           (list? v)
                                           (andmap path-string? v)
                                           (apply collection-path v #:fail (lambda (s) #f))))
                    (cond
                     [(hash-ref installed-libs k #f)
                      ;; just installed it, so keep it
                      (hash-set ht k v)]
                     [(and coll-path
                           ;; If we set up this collection, then the lib
                           ;; must be in the installed list if it's to be kept:
                           (let ([coll (map string->path v)])
                             (not
                              (for/or ([cc (in-list ccs-to-compile)])
                                (equal? coll (cc-collection cc))))))
                      ;; keep the lib
                      (hash-set ht k v)]
                     [else
                      ;; remove the lib
                      (define lib-path (build-dest-path target-dir (bytes->path-element k)))
                      (when (or (file-exists? lib-path)
                                (directory-exists? lib-path))
                        (setup-printf "deleting" (string-append what " ~a")
                                      (path->relative-string/* lib-path))
                        (delete-directory/files/hard lib-path))
                      ht])))
      (unless (equal? ht ht2)
        (setup-printf "updating" (format "~a list" what))
        (write-receipt-hash receipt-path ht2)))
    
    make-libs-step)

  (define make-foreign-libs-step
    (make-copy/move-step "foreign library"
                         "foreign libraries"
                         "foreign library setup"
                         'copy-foreign-libs
                         'move-foreign-libs
                         find-lib-dir
                         find-user-lib-dir
                         path->relative-string/lib
                         "libs.rktd" #t
                         (lambda (l)
                           (unless (list-of relative-path-string? l)
                             (error "entry is not a list of relative path strings:" l)))
                         build-path
                         this-platform?
                         (case (system-type)
                           [(macosx)
                            adjust-dylib-path/install]
                           [else void])
                         (case (system-type)
                           [(unix)
                            copy-file/install-elf-rpath]
                           [else copy-file])))

  (define make-shares-step
    (make-copy/move-step "shared file"
                         "shared files"
                         "share files setup"
                         'copy-shared-files
                         'move-shared-files
                         find-share-dir
                         find-user-share-dir
                         path->relative-string/share
                         "shares.rktd" #t
                         (lambda (l)
                           (unless (list-of relative-path-string? l)
                             (error "entry is not a list of relative path strings:" l)))
                         build-path
                         this-platform?
                         void
                         copy-file))

  (define make-mans-step
    (make-copy/move-step "man page"
                         "man pages"
                         "man page setup"
                         'copy-man-pages
                         'move-man-pages
                         find-man-dir
                         find-user-man-dir
                         path->relative-string/man
                         "mans.rktd" #f
                         (lambda (l)
                           (unless (list-of (lambda (p)
                                              (and (relative-path-string? p)
                                                   (filename-extension p)))
                                            l)
                             (error 
                              "entry is not a list of relative path strings,each with a non-empty extension:" 
                              l)))
                         (lambda (d n)
                           (build-path d 
                                       (bytes->path-element (bytes-append #"man" (filename-extension n)))
                                       n))
                         (lambda (info) #t)
                         void
                         copy-file))

  (define setup-delete-prefix #"raco-setup-delete-")

  (define (delete-directory/files/hard dest)
    (cond
     [(and (eq? 'windows (system-type))
           (file-exists? dest))
      ;; To handle DLLs that may be opened, try moving and then
      ;; deleting. The delete may well fail, but at least the
      ;; file will be out of the way for another try.
      (define-values (base name dir?) (split-path dest))
      (define delete-dest (build-path base
                                      (bytes->path-element
                                       (bytes-append
                                        setup-delete-prefix
                                        (path-element->bytes name)))))
      (rename-file-or-directory dest delete-dest #t)
      (try-delete-file delete-dest)]
     [else
      (delete-directory/files dest #:must-exist? #f)]))

  (define (try-delete-file f)
    (with-handlers ([exn:fail:filesystem?
                     (lambda (exn)
                       (setup-printf
                        "WARNING"
                        "error deleteing file: ~a"
                        (exn-message exn)))])
      (delete-file f)))

  (define (clean-previous-delete-failures lib-dir path->relative-string/*)
    (when (and (eq? 'windows (system-type))
	       (directory-exists? lib-dir))
      (for ([f (in-list (directory-list lib-dir))])
        (define bstr (path-element->bytes f))
        (when (equal? (subbytes bstr 0 (min (bytes-length setup-delete-prefix)
                                            (bytes-length bstr)))
                      setup-delete-prefix)
          (define p (build-path lib-dir f))
          (setup-printf "deleting" (path->relative-string/* p))
          (try-delete-file (build-path lib-dir f))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       Package-dependency checking         ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (do-check-package-dependencies)
    (setup-printf #f (format "--- checking package dependencies ---"))
    (unless (check-package-dependencies (map cc-path ccs-to-compile)
                                        (map cc-collection ccs-to-compile)
                                        (map cc-main? ccs-to-compile)
                                        ;; If "test" or "scribblings" is this collection's name,
                                        ;; then it's build-mode code, otherwise it's test mode:
                                        (let ([tests-path (string->path "tests")]
                                              [scribblings-path (string->path "scribblings")])
                                          (for/list ([cc (in-list ccs-to-compile)])
                                            (and (cc-collection cc) ; #f for a PLaneT package
                                                 (if (or (member tests-path (cc-collection cc))
                                                         (member scribblings-path (cc-collection cc)))
                                                     'build
                                                     'run))))
                                        setup-printf setup-fprintf
                                        (check-unused-dependencies)
                                        (fix-dependencies)
                                        (verbose)
                                        (not no-specific-collections?)
                                        (always-check-dependencies))
      (set! exit-code 1)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; setup Body                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setup-printf "version" "~a [~a]" (version) (system-type 'gc))
  (setup-printf "installation name" "~a" (get-installation-name))
  (setup-printf "variants" "~a" (string-join (map symbol->string (available-mzscheme-variants)) ", "))
  (setup-printf "main collects" "~a" main-collects-dir)
  (setup-printf "collects paths" (if (null? (current-library-collection-paths)) " empty!" ""))
  (for ([p (current-library-collection-paths)])
    (setup-printf #f "  ~a" p))
  (setup-printf "main pkgs" "~a" (find-pkgs-dir))
  (setup-printf "pkgs paths" "")
  (for ([p (get-pkgs-search-dirs)])
    (setup-printf #f "  ~a" p))
  (setup-printf #f "  ~a" (find-user-pkgs-dir))
  (setup-printf "links files" "")
  (for ([p (get-links-search-files)])
    (setup-printf #f "  ~a" p))
  (setup-printf #f "  ~a" (find-user-links-file))
  (setup-printf "main docs" "~a" (find-doc-dir))

  (when (and (not (null? (archives))) no-specific-collections?)
    (done))

  (when (make-info-domain) (make-info-domain-step))

  (when (clean) (clean-step))
  (when (make-zo)
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (do-install-part 'pre)

  (when (make-foreign-libs)
    (make-foreign-libs-step)
    (make-shares-step))

  (when (make-zo) (make-zo-step))

  (when (make-launchers) (make-launchers-step))
  (when (make-launchers) 
    (unless (eq? 'windows (system-type))
      (make-mans-step)))

  (when make-docs?
    (make-docs-step))
  (when (doc-pdf-dest) (doc-pdf-dest-step))
  
  (do-install-part 'general)
  (do-install-part 'post)

  (when (and (check-dependencies)
             (or no-specific-collections?
                 (always-check-dependencies)))
    (do-check-package-dependencies))

  (done))
