(module util mzscheme
  
  (require "config.ss"
           "planet-archives.ss"
           
           "private/planet-shared.ss"
           "private/linkage.ss"

           (lib "pack.ss" "setup")
           (lib "contract.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "plt-single-installer.ss" "setup"))

  #| The util collection provides a number of useful functions for interacting with the PLaneT system. |#
  
  (provide 
   current-cache-contents
   current-linkage
   make-planet-archive
   get-installed-planet-archives
   remove-pkg
   unlink-all)

  ;; current-cache-contents : -> ((string ((string ((nat (nat ...)) ...)) ...)) ...)
  ;; returns the packages installed in the local PLaneT cache
  (define (current-cache-contents)
    (cdr (tree->list (repository-tree))))
  
  ;; get-installed-package : string string nat nat -> PKG | #f
  ;; gets the package associated with this package, if any
  (define (get-installed-package owner name maj min)
    (lookup-package (make-pkg-spec name maj min min (list owner) #f) (CACHE-DIR)))
  
  ;; just so it will be provided
  (define unlink-all remove-all-linkage!)
  
  ;; to remove:
  ;;   -- setup-plt -c the package
  ;;   -- remove relevant infodomain cache entries
  ;;   -- delete files from cache directory
  ;;   -- remove any existing linkage for package
  ;; returns #t if the removal worked; #f if no package existed.
  (define (remove-pkg owner name maj min)
    (let ((p (get-installed-package owner name maj min)))
      (and p
           (let ((path (pkg-path p)))
             (with-logging 
              (LOG-FILE)
              (lambda () 
                (printf "\n============= Removing ~a =============\n" (list owner name maj min))
                (clean-planet-package path (list owner name '() maj min))))
             (remove-infodomain-entries path)
             (delete-directory/files path)
             (trim-directory (CACHE-DIR) path)
             (remove-linkage-to! p)))))
    
  ;; this really should go somewhere else. But what should setup's behavior be
  ;; when a package is cleaned? should it clear info-domain entries out? I think
  ;; no; an uncompiled package isn't necessarily not to be indexed and so on.
  ;; remove-infodomain-entries : path -> void
  (define (remove-infodomain-entries path)
    (let* ([pathbytes (path->bytes path)]
           [cache-file (build-path (PLANET-DIR) "cache.ss")]
           [cache-lines (with-input-from-file cache-file read)])
      (with-output-to-file cache-file
       (lambda ()
         (if (pair? cache-lines)
             (write (filter (lambda (line) (not (and (pair? line) (equal? (car line) pathbytes)))) cache-lines))
             (printf "\n")))
        'truncate/replace)))
  
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
    (let* ((links (with-input-from-file (LINKAGE-FILE) read-all))
           (buckets (categorize caar links)))
      (map
       (lambda (x) (cons (car x) (map (lambda (y) (drop-last (cadr y))) (cdr x))))
       buckets)))
  
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
         (pack archive-name
               "archive" 
               (list ".")
               null
               std-filter
               #t
               'file
               #f
               #f))
       (normalize-path archive-name)])))