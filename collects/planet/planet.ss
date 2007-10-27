(module planet mzscheme
      #|
This module contains code that implements the `planet' command-line tool.
  
PLANNED FEATURES:
  
2. Remove a package from the cache
3. Disable a package without removing it (disabling meaning
   that if it's a tool it won't start w/ DrScheme, etc)
4. Examine and alter linkage table
|#
  (require (lib "cmdline.ss")
           (lib "string.ss")
           (lib "file.ss")
           (only (lib "list.ss") sort)
           (lib "url.ss" "net")
           (lib "match.ss")
           
           "config.ss"
           "private/planet-shared.ss"
           "resolver.ss" ;; the code I need should be pulled out into a common library
           "util.ss") 
  
  (define actions '())
  
  (define (start)

    (make-directory* (PLANET-DIR))
    (make-directory* (CACHE-DIR))
    
    (command-line
     "planet"
     (current-command-line-arguments)
     (once-each
      (("--force")
       ""
       "Used in conjunction with --create-package; force a package to be"
       "created even its info.ss file contains errors."
       (force-package-building? #t)))
     (once-any
      (("-f" "--file")
       plt-file owner maj min
       ""
       "Install local file <plt-file> as though it had been downloaded from"
       "the planet server.  The installed package has path"
       "  (planet (<owner> <plt-file's filename> <maj> <min>))"
       (set! actions (cons (lambda () (install-plt-file plt-file owner maj min)) actions)))
      (("-c" "--create-archive")
       path
       ""
       "Create a PLaneT archive in the current directory"
       "whose contents are the directory <path>"
       (set! actions (cons (lambda () (do-archive path)) actions)))
      (("-i" "--install")
       owner pkg maj min
       ""
       "Download and install the package"
       "  (require (planet \"file.ss\" (<owner> <pkg> <maj> <min>)))"
       "would install"
       (set! actions (cons (lambda () (download/install owner pkg maj min)) actions)))
      (("-d" "--download")
       owner pkg maj min
       ""
       "Download the given package file without installing it"
       (set! actions (cons (lambda () (download/no-install owner pkg maj min)) actions)))
      (("-r" "--remove")
       owner pkg maj min
       ""
       "Remove the specified package from the local cache"
       (set! actions (cons (lambda () (remove owner pkg maj min)) actions)))
      (("-e" "--erase")
       owner pkg maj min
       ""
       "Erase the specified package, removing it as -r does and "
       "eliminating the package's distribution file from the "
       "uninstalled-package cache"
       (set! actions (cons (lambda () (erase owner pkg maj min)) actions)))
      (("-U" "--unlink-all")
       ""
       "Clear the linkage table, unlinking all packages and allowing upgrades"
       (set! actions (cons unlink-all actions)))
      (("-p" "--packages")
       ""
       "List the packages installed in the local cache"
       (set! actions (cons show-installed-packages actions)))
      (("-l" "--linkage")
       ""
       "List the current linkage table"
       (set! actions (cons show-linkage actions)))
      
      (("-a" "--associate")
       owner pkg maj min path
       ""
       "Create a development link between the specified package specifier "
       "and the specified directory name"
       (set! actions (cons (lambda () (add-hard-link-cmd owner pkg maj min path)) actions)))
      
      (("-u" "--unassociate")
       owner pkg maj min
       ""
       "Remove any development link associated with the specified package"
       (set! actions (cons (lambda () (remove-hard-link-cmd owner pkg maj min)) actions)))
      
      (("--url")
       owner pkg maj min
       "Get a URL for the given package"
       (set! actions (cons (lambda () (get-download-url owner pkg maj min)) actions)))
      
      (("--unpack")
       plt-file target
       "Unpack the contents of the given package into the given directory without installing"
       (set! actions (cons (lambda () (do-unpack plt-file target)) actions)))
                                       
      ;; unimplemented so far:
      #;(("-u" "--unlink")
         module
         "Remove all linkage the given module has, forcing it to upgrade"
         ...)))

    (cond
      ; make showing the installed packages the default thing to do.
      [(null? actions) (show-installed-packages)]
      [else (for-each (lambda (f) (f)) actions)]))
  
  ;; ============================================================
  ;; FEATURE IMPLEMENTATIONS
  
  (define (fail s . args)
    (raise (make-exn:fail (apply format s args) (current-continuation-marks))))
  
  (define (download/install owner name majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [full-pkg-spec (pkg-spec->full-pkg-spec (list owner name maj min) #f)])
      (when (get-package-from-cache full-pkg-spec)
        (fail "No package installed (cache already contains a matching package)"))
      (unless (download/install-pkg owner name maj min)
        (fail "Could not find matching package"))))
  
  (define (download/no-install owner pkg majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [full-pkg-spec (pkg-spec->full-pkg-spec (list owner pkg maj min) #f)])
      (when (file-exists? pkg)
        (fail "Cannot download, there is a file named ~a in the way" pkg))
      (match (download-package full-pkg-spec)
        [(#t path maj min) 
         (copy-file path pkg)
         (printf "Downloaded ~a package version ~a.~a\n" pkg maj min)]
	[_ 
        (fail "Could not find matching package")])))
  
  ;; params->full-pkg-spec : string string string string -> pkg
  ;; gets a full package specifier for the given specification
  (define (params->full-pkg-spec ownerstr pkgstr majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (let* ([spec (list ownerstr pkgstr maj min)]
             [fullspec (pkg-spec->full-pkg-spec spec #f)])
        (unless fullspec (fail "invalid spec: ~a" fullspec))
        fullspec)))
    
  (define (install-plt-file filestr owner majstr minstr)
    (unless (file-exists? filestr) (fail "File does not exist: ~a" filestr))
    (let* ([file (normalize-path filestr)]
           [name (let-values ([(base name dir?) (split-path file)]) (path->string name))]
           [fullspec (params->full-pkg-spec owner name majstr minstr)])
      (install-pkg fullspec file (pkg-spec-maj fullspec) (pkg-spec-minor-lo fullspec))))
  
  (define (do-archive p)
    (unless (directory-exists? p)
      (fail "No such directory: ~a" p))
    (make-planet-archive (normalize-path p)))
  
  (define (remove owner pkg majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (with-handlers ([exn:fail:planet? (λ (e) (fail (exn-message e)))]) 
        (remove-pkg owner pkg maj min))))
  
  (define (erase owner pkg majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (with-handlers ([exn:fail:planet? (λ (e) (fail (exn-message e)))])
        (erase-pkg owner pkg maj min))))
        
  (define (show-installed-packages)
    (let ([normal-packages (get-installed-planet-archives)]
          [devel-link-packages (get-hard-linked-packages)])
      
      (define (show-normals)
        (printf "Normally-installed packages:\n")
        (for-each 
         (lambda (l) (apply printf "  ~a\t~a\t~a ~a\n" l))
         (sort-by-criteria 
          (map (lambda (x) (match x [(_ owner pkg _ maj min) (list owner pkg maj min)])) normal-packages)
          (list string<? string=?)
          (list string<? string=?)
          (list < =)
          (list < =))))
      
      (define (show-devel-links)
        (printf "Development links:\n")
        (for-each 
         (lambda (l) (apply printf "  ~a\t~a\t~a ~a\n    --> ~a\n" l))
         (sort-by-criteria 
          (map 
           (lambda (x) (match x [(dir owner pkg _ maj min) (list owner pkg maj min (path->string dir))]))
           devel-link-packages)
          (list string<? string=?)
          (list string<? string=?)
          (list < =)
          (list < =)
          (list string<? string=?))))
      
      (cond
        [(and (pair? normal-packages) (pair? devel-link-packages))
         (begin 
           (show-normals)
           (newline)
           (show-devel-links))]
        [(pair? normal-packages) (show-normals)]
        [(pair? devel-link-packages) (show-devel-links)]
        [else (printf "No packages installed.\n")])))
      
  
  (define (show-linkage)
    (for-each
     (lambda (module)
       (printf "  ~a:\n" (car module))
       (for-each 
        (lambda (link) (apply printf "    ~a\t~a\t~a ~a\n" link))
        (cdr module)))
     (sort (current-linkage)
           (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))))))
  
  (define (add-hard-link-cmd ownerstr pkgstr majstr minstr pathstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [path (string->path pathstr)])
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (add-hard-link ownerstr pkgstr maj min path)))
  
  (define (remove-hard-link-cmd ownerstr pkgstr majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)])
      (remove-hard-link ownerstr pkgstr maj min)))
      
  (define (get-download-url ownerstr pkgstr majstr minstr)
    (let ([fps (params->full-pkg-spec ownerstr pkgstr majstr minstr)])
      (printf "~a\n" (url->string (pkg->download-url fps)))))
  
  (define (do-unpack plt-file target)
    (unless (file-exists? plt-file)
      (fail (format "The specified file (~a) does not exist" plt-file))) 
    (let ([file (normalize-path plt-file)])
      (unpack-planet-archive file target)))
  
  ;; ------------------------------------------------------------
  ;; Utility
    
  (define (sort-by-criteria l . criteria)
    (sort l
          (lambda (a b)
            (let loop ((a a) (b b) (c criteria))
              (cond
               [(null? a) #f]
               [((caar c) (car a) (car b)) #t]
               [(not ((cadar c) (car a) (car b))) #f]
               [else (loop (cdr a) (cdr b) (cdr c))])))))


  ;; ============================================================
  ;; start the program
  
  (with-handlers ([exn:fail? 
                   (lambda (e) 
                     (fprintf (current-error-port) "~a\n" (exn-message e))
                     (exit 1))])
    (start)))
