(module util mzscheme
  
  (require "config.ss"
           "planet-archives.ss"
           
           "private/planet-shared.ss"
           "private/linkage.ss"
           "resolver.ss"
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "contract.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "pack.ss" "setup")
           (lib "plt-single-installer.ss" "setup")
           (lib "getinfo.ss" "setup")
           (lib "unpack.ss" "setup")
           (lib "etc.ss"))

  #| The util collection provides a number of useful functions for interacting with the PLaneT system. |#
  
  (provide 
   
   current-cache-contents
   current-linkage
   make-planet-archive
   unpack-planet-archive
   force-package-building?
   get-installed-planet-archives
   get-hard-linked-packages
   unlink-all
   lookup-package-by-keys
   resolve-planet-path
   (struct exn:fail:planet ()))
  
  (provide/contract
   [download/install-pkg
    (-> string? string? natural-number/c natural-number/c (union pkg? false/c))]
   [add-hard-link 
    (-> string? string? natural-number/c natural-number/c path? void?)]
   [remove-hard-link 
    (-> string? string? natural-number/c natural-number/c void?)]
   [remove-pkg
    (-> string? string? natural-number/c natural-number/c void?)]
   [erase-pkg
    (-> string? string? natural-number/c natural-number/c void?)])

  ;; download/install-pkg : string string nat nat -> pkg | #f
  (define (download/install-pkg owner name maj min)
    (let* ([pspec (pkg-spec->full-pkg-spec (list owner name maj min) #f)]
           [upkg (get-package-from-server pspec)])
      (cond
        [(uninstalled-pkg? upkg)
         (pkg-promise->pkg upkg)]
        [else #f])))
  
  ;; current-cache-contents : -> ((string ((string ((nat (nat ...)) ...)) ...)) ...)
  ;; returns the packages installed in the local PLaneT cache
  (define (current-cache-contents)
    (cdr (tree->list (repository-tree))))
  
  ;; just so it will be provided
  (define unlink-all remove-all-linkage!)
  
  ;; to remove:
  ;;   -- setup-plt -c the package
  ;;   -- remove relevant infodomain cache entries
  ;;   -- delete files from cache directory
  ;;   -- remove any existing linkage for package
  ;; returns void if the removal worked; raises an exception if no package existed.
  
  (define-struct (exn:fail:planet exn:fail) ())
  
  (define (remove-pkg owner name maj min)
    (let ((p (get-installed-package owner name maj min)))
      (unless p
        (raise (make-exn:fail:planet "Could not find package" (current-continuation-marks))))
      (unless (normally-installed-pkg? p)
        (raise (make-exn:fail:planet "Not a normally-installed package, can't remove" (current-continuation-marks))))

      (let ((path (pkg-path p)))
        (with-logging 
         (LOG-FILE)
         (lambda () 
           (printf "\n============= Removing ~a =============\n" (list owner name maj min))
           (clean-planet-package path (list owner name '() maj min))))
        (erase-metadata p)
        (delete-directory/files path)
        (trim-directory (CACHE-DIR) path)
        (void))))
  
  ;; erase-metadata : pkg -> void
  ;; clears out any references to the given package in planet's metadata files
  ;; (i.e., linkage and info.ss cache; not hard links which are not considered metadata)
  (define (erase-metadata p)
    (remove-infodomain-entries (pkg-path p))
    (remove-linkage-to! p))
    
  ;; this really should go somewhere else. But what should setup's behavior be
  ;; when a package is cleaned? should it clear info-domain entries out? I think
  ;; no; an uncompiled package isn't necessarily not to be indexed and so on.
  ;; remove-infodomain-entries : path -> void
  (define (remove-infodomain-entries path)
    (let* ([pathbytes (path->bytes path)]
           [cache-file (build-path (PLANET-DIR) "cache.ss")])
      (when (file-exists? cache-file)
        (let ([cache-lines (with-input-from-file cache-file read)])
          (call-with-output-file cache-file
            (λ (op)
              (if (pair? cache-lines)
                  (write (filter
                          (λ (line) 
                            (not 
                             (and
                              (pair? line)
                              (or (not (directory-exists? (bytes->path (car line))))
                                  (subpath? path (bytes->path (car line)))))))                              
                          cache-lines)
                         op)
                  (fprintf op "\n")))
            'truncate/replace)))))
  
  ;; subpath? : path path -> boolean
  ;; determines if p1 is a subpath of p2. Both paths must actually exist on the filesystem
  (define (subpath? p1 p2)
    (let ([full-p1 (explode-path (normalize-path p1))]
          [full-p2 (explode-path (normalize-path p2))])
      (sublist? full-p1 full-p2 (o2 bytes=? path->bytes))))

  ;; o2 : (X X -> Y) (Z -> X) -> (Z Z -> Y)
  ;; "compose-two"
  (define (o2 a b) (λ (x y) (a (b x) (b y))))

  ;; sublist? : (listof X) (listof X) (X X -> boolean) -> boolean
  ;; determine if l1 is a sublist of l2, using = as the comparison operator for elements
  (define (sublist? l1 l2 =)
    (cond
      [(null? l1) #t]
      [(null? l2) #f]
      [(= (car l1) (car l2)) (sublist? (cdr l1) (cdr l2) =)]
      [else #f]))
  
  (define (erase-pkg owner name maj min)
    (let* ([uninstalled-pkg-dir
            (build-path (UNINSTALLED-PACKAGE-CACHE) owner name (number->string maj) (number->string min))]
           [uninstalled-pkg-file (build-path uninstalled-pkg-dir name)]
           [uninstalled-file-exists? (file-exists? uninstalled-pkg-file)])
      (when uninstalled-file-exists?
        (delete-file uninstalled-pkg-file)
        (trim-directory (UNINSTALLED-PACKAGE-CACHE) uninstalled-pkg-dir))
      (with-handlers ([exn:fail:planet? 
                       (λ (e) (if uninstalled-file-exists? 
                                  ;; not really a failure, just return
                                  (void)
                                  (raise e)))])
        (remove-pkg owner name maj min))))

  ;; listof X * listof X -> nonempty listof X
  ;; returns de-prefixed version of l2 if l1 is a proper prefix of l2; 
  ;; signals an error otherwise.
  (define (drop-common-base list1 list2)
    (let loop ((l1 list1) (l2 list2))
      (cond
        [(null? l2)
         (error 'drop-common-base "root ~s is not a prefix of stem ~s" list1 list2)]
        [(null? l1) l2]
        [(not (equal? (car l1) (car l2)))
         (error 'drop-common-base "root ~s is not a prefix of stem ~s" list1 list2)]
        [else (loop (cdr l1) (cdr l2))])))
  
  ;; pathify-list : path (listof path) -> listof path
  ;; given a base and a list of names, interprets each name as a subdirectory
  ;; of the previous, starting with base, and returns a list. (This list
  ;; is in reverse order, so the deepest subdirectory is returned first)
  (define (pathify-list root dirs)
    (let loop ((base root) (dirs dirs) (acc '()))
      (cond
        [(null? dirs) acc]
        [else
         (let ((new (build-path base (car dirs))))
           (loop new (cdr dirs) (cons new acc)))])))
  
  ;; directory-empty? path -> bool
  ;; #t iff the given directory contains no subdirectories of files
  (define (directory-empty? dir)
    (null? (directory-list dir)))
  
  ;; trim-directory path path -> void
  ;; deletes nonempty directories starting with stem and working down to root
  (define (trim-directory root stem)
    (let* ([rootl (explode-path root)]
           [steml (explode-path stem)]
           [extras (cdr (pathify-list root (drop-common-base rootl steml)))])
      (let loop ((dirs extras))
        (cond
          [(null? dirs) (void)]
          [(directory-empty? (car dirs))
           (delete-directory (car dirs))
           (loop (cdr dirs))]
          [else (void)]))))

  ;; current-linkage : -> ((symbol (package-name nat nat) ...) ...)
  ;; gives the current "linkage table"; a table that links modules to particular versions
  ;; of planet requires that satisfy those linkages
  (define (current-linkage)
    (let* ((links 
            (if (file-exists? (LINKAGE-FILE))
                (with-input-from-file (LINKAGE-FILE) read-all)
                '()))
           (buckets (categorize caar links)))
      (map
       (lambda (x) (cons (car x) (map (lambda (y) (drop-last (cadr y))) (cdr x))))
       buckets)))
  
  ;; regexp->filter : (string | regexp) -> (path -> bool)
  ;; computes a filter that accepts paths that match the given regexps and rejects other paths
  (define (regexp->filter re-s)
    (let ([re (cond
                [(string? re-s) (regexp re-s)]
                [(regexp? re-s) re-s]
                [else (error 'regexp->filter "not a regular expression")])])
      (lambda (p) (regexp-match re (path->bytes p)))))
  
  (define force-package-building? (make-parameter #f))
  
  ;; make-planet-archive: directory [file] -> file
  ;; Makes a .plt archive file suitable for PLaneT whose contents are
  ;; all files in the given directory and returns that file's name.
  ;; If the optional filename argument is provided, that filename will 
  ;; be used as the output file's name.
  (define make-planet-archive
    (case-lambda
      [(dir) 
       (let-values ([(path name must-be-dir?) (split-path dir)])
         (make-planet-archive 
          dir 
          (build-path (normalize-path (current-directory))
                      (string-append (path->string name) ".plt"))))]
      [(dir archive-name)
       (parameterize ((current-directory dir))
         (let ([announcements '()]
               [warnings '()]
               [critical-errors '()])
           (check-info.ss-sanity 
            dir
            (λ (msg . args) (set! announcements (cons (apply format msg args) announcements)))
            (λ (bad) (set! warnings (cons bad warnings)))
            (λ (err) (set! critical-errors (cons err critical-errors))))
           
           (unless 
               (or (null? critical-errors)
                   (force-package-building?))
             (error '|PLaneT packager| "~a Refusing to continue packaging." (car critical-errors)))
           
           (pack archive-name
                 "archive" 
                 (list ".")
                 null
                 (if (PLANET-ARCHIVE-FILTER)
                     (regexp->filter (PLANET-ARCHIVE-FILTER))
                     std-filter)
                 #t
                 'file
                 #f
                 #f)
         
           (for-each display (reverse announcements))
           (newline)
           (for-each
            (λ (s) (fprintf (current-error-port) "WARNING:\n\t~a\n" s))
            (reverse warnings)))
         
         (normalize-path archive-name))]))
  
  (define (unpack-planet-archive plt-file target)
    (parameterize ([current-directory target])
      (unpack plt-file)))
  
  ;; check-info.ss-sanity : path (string -> void) (string -> void) (string -> void) -> void
  ;; gets all the info.ss fields that planet will use (using the info.ss file
  ;; from the current directory) and calls the announce, warn, and fail functions with strings
  ;; that describe how PLaneT sees the info.ss file. NOTA BENE: if this function calls fail, it may 
  ;; also warn on the same field, and the warning may not make sense. This is based on the
  ;; assumption that errors will be turned into some kind of critical failure that obliterates
  ;; all the other information produced.
  (define (check-info.ss-sanity dir announce warn fail)
    (with-handlers ([exn:fail:read? 
                     (λ (e) (fail (format "Package has an unreadable info.ss file. ~a" (exn-message e))))]
                    [exn:fail:syntax? 
                     (λ (e) (fail (format "Package's info.ss has an syntactically ill-formed info.ss file: ~a" (exn-message e))))]) 
      (let ([i* (get-info/full dir)])
        (cond
          [(not i*) 
           (warn "Package has no info.ss file. This means it will not have a description or documentation on the PLaneT web site.")]
          [else
           (let ([i (λ (field) (i* field (λ () #f)))])
             (checkinfo i fail
                        [name     ; field name 
                         string?  ; check
                         (announce "Name: ~a\n" name)  ; success action
                         (warn "Package's info.ss file has no name field. Without a name, PLT Scheme will not compile your package.") ;failure action
                         ]
                        [blurb 
                         (λ (b) (and (list? b) (andmap xexpr? b)))
                         (announce "Package blurb: ~s\n" blurb)
                         (unless blurb
                           (warn "Package's info.ss does not contain a blurb field. Without a blurb field, the package will have no description on planet.plt-scheme.org."))]
                        [release-notes 
                         (λ (b) (and (list? b) (andmap xexpr? b)))
                         (announce "Release notes: ~s\n" release-notes)
                         (unless release-notes
                           (warn "Package's info.ss does not contain a release-notes field. Without a release-notes field, the package will not have any listed release information on planet.plt-scheme.org beyond the contents of the blurb field."))]
                        [categories
                         (λ (s) (and (list? s) (andmap symbol? s)))
                         (cond
                           [(ormap illegal-category categories)
                            =>
                            (λ (bad-cat)
                              (fail (format "Package's info.ss file contains illegal category \"~a\". The legal categories are: ~a\n" 
                                            bad-cat
                                            legal-categories)))]
                           [else (announce "Categories: ~a\n" categories)])
                         (unless categories
                           (warn "Package's info.ss file does not contain a category listing. It will be placed in the Miscellaneous category."))]
                        [doc.txt
                         string?
                         (announce "doc.txt file: ~a\n" doc.txt)
                         (unless doc.txt
                           (warn "Package's info.ss does not contain a doc.txt entry. Without a doc.txt entry, the package will not have any documentation on planet.plt-scheme.org."))]
                        [html-docs
                         (lambda (s) (and (list? s) (andmap string? s)))
                         (announce "HTML documentation: yes\n")]
                        [homepage 
                         string?
                         (cond
                           [(url-string? homepage)
                            (announce "Home page: ~a\n" homepage)]
                           [else
                            (fail (format "The value of the package's info.ss homepage field, ~s, does not appear to be a legal URL." homepage))])]
                        [primary-file
                         (λ (x) (or (string? x) (and (list? x) (andmap string? x))))
                         (begin
                           (cond
                             [(string? primary-file) 
                              (unless (file-in-current-directory? primary-file)
                                (warn (format "Package's info.ss primary-file field is ~s, a file that does not exist in the package." 
                                              primary-file)))]
                             [(pair? primary-file)
                              (let ([bad-files (filter (λ (f) (not (file-in-current-directory? f))) primary-file)])
                                (unless (null? bad-files)
                                  (warn (format "Package's info.ss primary-file field is ~s, which contains non-existant files ~s."
                                                primary-file bad-files))))])
                           (announce "Primary file: ~a\n" primary-file))
                         (unless primary-file
                           (warn "Package's info.ss does not contain a primary-file field. The package's listing on planet.plt-scheme.org will not have a valid require line for your package."))]
                        [required-core-version 
                         core-version?
                         (announce "Required mzscheme version: ~a\n" required-core-version)]
                        [version
                         string?
                         (announce "Version description: ~a\n" version)]))]))))
  
  ;; legal-categories : (listof symbol)
  (define legal-categories
    '(devtools net media xml datastructures io scientific
      system ui metaprogramming planet misc))

  ;; legal-category : symbol -> boolean
  ;; determine if the given symbol is a legal category
  (define (legal-category? x) (memq x legal-categories))
  
  ;; illegal-category : symbol -> (union symbol false)
  ;; returns #f if the symbol is a legal category, or the symbol itself if it isn't
  (define (illegal-category s) (if (legal-category? s) #f s)) 

  ;; url-string? : string -> boolean
  ;; determines if the given string is a reasonable homepage url
  (define (url-string? s)
    (and (string? s)
         (let ([u (string->url s)])
           (and (url-scheme u)
                (url-host u)))))
  
  ;; file-in-current-directory? : string -> boolean
  ;; determines if the given string represents a file in the current directory
  (define (file-in-current-directory? f)
    (and (string? f) (file-exists? f)))
  
  ;; core-version : string -> boolean
  ;; determines if the given string is something that (version) could've produced
  (define (core-version? s)
    (and (string? s)
         (regexp-match #rx"^[0-9]+(\\.[0-9]*)?$" s)))
            
  ;; checkinfo: syntax
  ;; given an info.ss function, a failure function, and a bunch of fields to check,
  ;; goes through the checklist calling either the success or the failure branch
  ;; of each check as appropriate
  (define-syntax checkinfo
    (syntax-rules ()
      [(checkinfo fn fail clauses ...)
       (let ([fn* fn] [fail* fail])
         (checkinfo* () fn* fail* clauses ...))]))
  
  (define-syntax checkinfo*
    (syntax-rules ()
      [(checkinfo* () fn fail) (void)]
      [(checkinfo* (handler1 handler ...) fn fail) (begin handler1 handler ...)]
      [(checkinfo* (handler ...) fn fail [id check on-success] clauses ...)
       (checkinfo* (handler ...) fn fail [id check on-success void] clauses ...)]
      [(checkinfo* (handler ...) fn fail [id check on-success on-fail] clauses ...)
       (checkinfo*
        (handler ...
         (let ([id (fn 'id)])
           (cond
             [id
              (let ([checked (check id)])
                (unless checked
                  on-fail
                  (fail (format "Package's info.ss contained a malformed ~a field." 'id)))
                on-success)]
             [else on-fail])))
         fn fail clauses ...)]))
  
  ;; ============================================================
  ;; HARD LINKS (aka development links)
  
  ;; add-hard-link : string string num num path -> void
  ;; adds an entry in the hard-links table associating the given
  ;; require spec to the given path
  (define (add-hard-link owner pkg-name maj min path)
    (unless (directory-exists? path)
      (if (file-exists? path)
          (error 'add-hard-link "Hard links must point to directories, not files")
          (fprintf (current-error-port) 
                   "Warning: directory ~a does not exist\n"
                   (path->string path))))
    (add-hard-link! pkg-name (list owner) maj min path))
  
  ;; remove-hard-link : string string num num -> void
  ;; removes any development association from the given package spec
  (define (remove-hard-link owner pkg-name maj min)
    (filter-link-table!
     (lambda (row) 
       (not (points-to? row pkg-name (list owner) maj min)))
     (lambda (row) 
       (let ([p (row->package row)])
         (when p
           (erase-metadata p))))))
  
  ;; ============================================================
  ;; VERSION INFO
  
  (provide this-package-version
           this-package-version-name
           this-package-version-owner
           this-package-version-maj
           this-package-version-min)
  
  (define-syntax (this-package-version stx)
    (syntax-case stx ()
      [(_)
       #`(this-package-version/proc 
          #,(datum->syntax-object stx `(,#'this-expression-source-directory)))]))
  
  (define-syntax define-getters
    (syntax-rules ()
      [(define-getters (name position) ...)
       (begin
         (define-syntax (name stx)
           (syntax-case stx ()
             [(name)
              #`(let ([p #,(datum->syntax-object stx `(,#'this-package-version))])
                  (and p (position p)))]))
         ...)]))
  
  (define-getters
    (this-package-version-name pd->name)
    (this-package-version-owner pd->owner)
    (this-package-version-maj pd->maj)
    (this-package-version-min pd->min))
  
  ;; ----------------------------------------
  
  (define (this-package-version/proc srcdir)
    (let* ([package-roots (get-all-planet-packages)]
           [thepkg (ormap (predicate->projection (contains-dir? srcdir))
                          package-roots)])
      (and thepkg (archive-retval->simple-retval thepkg))))

  ;; predicate->projection : #f \not\in X ==> (X -> boolean) -> (X -> X)
  (define (predicate->projection pred) (λ (x) (if (pred x) x #f)))
  
  ;; contains-dir? : path -> pkg -> boolean
  (define ((contains-dir? srcdir) alleged-superdir-pkg)
    (let* ([nsrcdir (normalize-path srcdir)]
           [nsuperdir (normalize-path (car alleged-superdir-pkg))]
           [nsrclist (explode-path nsrcdir)]
           [nsuperlist (explode-path nsuperdir)])
      (list-prefix? nsuperlist nsrclist)))
  
  (define (list-prefix? sup sub)
    (let loop ([sub sub]
               [sup sup])
      (cond
        [(null? sup) #t]
        [(equal? (car sup) (car sub))
         (loop (cdr sub) (cdr sup))]
        [else #f])))
  
  (define (archive-retval->simple-retval p)
    (list-refs p '(1 2 4 5)))
      
  (define-values (pd->owner pd->name pd->maj pd->min)
    (apply values (map (λ (n) (λ (l) (list-ref l n))) '(0 1 2 3))))
  
  (define (list-refs p ns)
    (map (λ (n) (list-ref p n)) ns)))
