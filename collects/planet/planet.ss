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
           (lib "match.ss")
           
           "config.ss"
           "resolver.ss" ;; the code I need should be pulled out into a common library
           "util.ss") 
  
  (define actions '())
  
  (define (start)

    (make-directory* (PLANET-DIR))
    (make-directory* (CACHE-DIR))
    
    (command-line
     "planet"
     (current-command-line-arguments)
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
           
      ;; unimplemented so far:
      #;(("-u" "--unlink")
         module
         "Remove all linkage the given module has, forcing it to upgrade"
         ...)))

    (for-each (lambda (f) (f)) actions))
  
  ;; ============================================================
  ;; FEATURE IMPLEMENTATIONS
  
  (define (fail s . args) 
    (raise (make-exn:fail (string->immutable-string (apply format s args)) (current-continuation-marks))))
  
  (define (download/install owner pkg majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [full-pkg-spec (pkg-spec->full-pkg-spec (list owner pkg maj min) #f)])
      (when (get-package-from-cache full-pkg-spec)
        (fail "No package installed (cache already contains a matching package)"))
      (unless (get-package-from-server full-pkg-spec)
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
  
  
  (define (install-plt-file filestr owner majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (unless (file-exists? filestr) (fail "File does not exist: ~a" filestr))
      (let* ([file (normalize-path filestr)]
             [name (let-values ([(base name dir?) (split-path file)]) (path->string name))]
             [spec (list owner name maj min)]
             [fullspec (pkg-spec->full-pkg-spec spec #f)])
        (unless spec (fail "invalid spec: ~a" spec))
        (install-pkg fullspec file maj min))))
  
  (define (do-archive p)
    (unless (directory-exists? p)
      (fail "No such directory: ~a" p))
    (make-planet-archive (normalize-path p)))
  
  (define (remove owner pkg majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (unless (remove-pkg owner pkg maj min)
        (fail "Could not find package"))))
        
  (define (show-installed-packages)
    (for-each 
     (lambda (l) (apply printf "  ~a\t~a\t~a ~a\n" l))
     (sort-by-criteria 
      (map (lambda (x) (match x [(_ owner pkg _ maj min) (list owner pkg maj min)])) (get-installed-planet-archives))
      (list string<? string=?)
      (list string<? string=?)
      (list < =)
      (list < =))))
  
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
      (add-hard-link ownerstr pkgstr maj min path)))
  
  (define (remove-hard-link-cmd ownerstr pkgstr majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)])
      (remove-hard-link ownerstr pkgstr maj min)))
      
  
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
