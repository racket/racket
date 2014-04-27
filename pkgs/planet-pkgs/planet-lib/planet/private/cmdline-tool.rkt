#lang racket/base
#|
This module contains code that implements the `planet' command-line tool.
  
PLANNED FEATURES:
* Disable a package without removing it (disabling meaning
  that if it's a tool it won't start w/ DrRacket, etc)
|#
  (require (only-in racket/path simple-form-path)
           net/url
           racket/file
           racket/match
           raco/command-name
           
           (only-in planet/resolver download?)
           planet/config
           planet/private/planet-shared
           planet/util
           planet/private/command)
  (provide start)
  
  (define erase? (make-parameter #f))
  (define displayer (make-parameter (λ () (show-installed-packages))))
  (define quiet-unlink? (make-parameter #f))

  (define (read-from-string str)
    (read
      (if (bytes? str) (open-input-bytes str) (open-input-string str))))

  
  (define (start raco?)

    (make-directory* (PLANET-DIR))
    (make-directory* (CACHE-DIR))
    (planet-logging-to-stdout #t)
    
    (svn-style-command-line
     #:program (short-program+command-name)
     #:argv (current-command-line-arguments)
     "The Racket command-line tool for manipulating packages installed by PLaneT."
     ["create" "create a PLaneT archive from a directory"
      "\nCreate a PLaneT archive in the current directory whose contents are the directory <path>."
      #:once-each
      [("-f" "--force") ("force a package to be created even if its info.rkt file contains"
                         "errors.")
       (force-package-building? #t)]
      #:args (path)
      (do-archive path)]
     ["install" "download and install a package"
      "
Download and install the package that
   (require (planet \"file.rkt\" (<owner> <pkg> <maj> <min>)))
would install"
      #:args (owner pkg maj min)
      (begin
        (verify-package-name pkg)
        (download/install owner pkg maj min))]
     ["remove" "remove a package from the local cache"
      "
Remove the specified package from the local cache, optionally also removing its distribution file"
      #:once-each 
      [("-e" "--erase") 
       ("also remove the package's distribution file from the"
        "uninstalled-package cache")
       (erase? #t)]
      #:args (owner pkg maj min)
      (begin
        (verify-package-name pkg)
        ((if (erase?) erase remove) owner pkg maj min))]
     ["show" "list the packages installed in the local cache"
      "\nList the packages installed in the local cache"
      #:once-any
      [("-p" "--packages") "show packages only (default)" (displayer show-installed-packages)]
      [("-l" "--linkage")  "show linkage table only"      (displayer show-linkage)]
      [("-a" "--all")      "show packages and linkage"    (displayer (λ () (show-installed-packages) (newline) (show-linkage)))]
      #:args ()
      ((displayer))]
     ["clearlinks" "clear the linkage table, allowing upgrades"
      "\nClear the linkage table, allowing upgrades"
      #:args ()
      (unlink-all)]
     ["fileinject" "install a local file to the planet cache"
       "
Install local file <plt-file> into the planet cache as though it had been downloaded from the planet server.  The installed package has path
  (planet (<owner> <plt-file's filename> <maj> <min>))"
       #:args (owner plt-file maj min)
       (install-plt-file plt-file owner maj min)]
     ["link" "create a package development link"
      "\nCreate a development link between the specified package specifier and the specified directory name"
      #:args (owner pkg maj min path)
      (begin
        (verify-package-name pkg)
        (add-hard-link-cmd owner pkg maj min path))]
     ["unlink" "remove a package development link"
      "\nRemove development link associated with the given package"
      #:once-each
      [("-q" "--quiet") "don't signal an error on nonexistent links" (quiet-unlink? #t)]
      #:args (owner pkg maj min)
      (begin
        (verify-package-name pkg)
        (remove-hard-link-cmd owner pkg maj min (quiet-unlink?)))]
     ["fetch" "download a package file without installing it"
       "\nDownload the given package file without installing it"
       #:args (owner pkg maj min)
       (begin
         (verify-package-name pkg)
         (download/no-install owner pkg maj min))]
     ["url" "get a URL for a package"
      "
Get a URL for the given package.
This is not necessary for normal use of planet, but may be helpful in some circumstances for retrieving packages."
      #:args (owner pkg maj min)
      (begin
        (verify-package-name pkg)
        (get-download-url owner pkg maj min))]
     ["open" "unpack the contents of a package"
      "
Unpack the contents of the given package into the given directory without installing.
This command is not necessary for normal use of planet. It is intended to allow you to inspect package contents offline without needing to install the package."
      #:args (plt-file target)
      (do-unpack plt-file target)]
     
     ["structure" "display the structure of a .plt archive"
      "\nPrint the structure of the PLaneT archive named by <plt-file> to the standard output port.
This command does not unpack or install the named .plt file."
      #:args (plt-file)
      (do-structure plt-file)]
     
     ["print" "display a file within a .plt archive"
      "\nPrint the contents of the file named by <path>, which must be a relative path within the PLaneT archive named by <plt-file>, to the standard output port.
This command does not unpack or install the named .plt file."
      #:args (plt-file path)
      (do-display plt-file path)]

     ;; unimplemented so far:
     #;(("-u" "--unlink")
         module
         "Remove all linkage the given module has, forcing it to upgrade"
         ...)))

  (define (verify-package-name pkg)
    (unless (regexp-match #rx"\\.plt$" pkg)
      (eprintf "Expected package name to end with '.plt', got: ~a\n" pkg)
      (exit 1)))
  
  
  ;; ============================================================
  ;; FEATURE IMPLEMENTATIONS
  
  (define (fail s . args)
    (raise (make-exn:fail (apply format s args) (current-continuation-marks))))
  (define (warn s . args)
    (apply printf s args)
    (newline))
  
  (define (download/install owner name majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [full-pkg-spec (get-package-spec owner name maj min)])
      (if (get-package-from-cache full-pkg-spec)
          (warn "No package installed (cache already contains a matching package)")
          (unless (download/install-pkg owner name maj min)
            (fail "Could not find matching package")))))
  
  (define (download/no-install owner pkg majstr minstr)
    (parameterize ([download? #t])
      (let* ([maj (read-from-string majstr)]
             [min (read-from-string minstr)]
             [full-pkg-spec (get-package-spec owner pkg maj min)])
        (when (file-exists? pkg)
          (fail "Cannot download, there is a file named ~a in the way" pkg))
        (match (download-package full-pkg-spec)
          [(list #t path maj min) 
           (copy-file path pkg)
           (printf "Downloaded ~a package version ~a.~a\n" pkg maj min)]
          [_ 
           (fail "Could not find matching package")]))))
  
  ;; params->full-pkg-spec : string string string string -> pkg
  ;; gets a full package specifier for the given specification
  (define (params->full-pkg-spec ownerstr pkgstr majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (let* ([fullspec (get-package-spec ownerstr pkgstr maj min)])
        (unless fullspec (fail "invalid spec: ~a" fullspec))
        fullspec)))
    
  (define (install-plt-file filestr owner majstr minstr)
    (unless (file-exists? filestr) (fail "File does not exist: ~a" filestr))
    (let* ([file (simple-form-path filestr)]
           [name (let-values ([(base name dir?) (split-path file)]) (path->string name))]
           [fullspec (params->full-pkg-spec owner name majstr minstr)])
      (install-pkg fullspec file (pkg-spec-maj fullspec) (pkg-spec-minor-lo fullspec))))
  
  (define (do-archive p)
    (unless (directory-exists? p)
      (fail "No such directory: ~a" p))
    (make-planet-archive (simple-form-path p)))
  
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
         (lambda (l) (apply printf "  ~a \t~a \t~a ~a\n" l))
         (sort-by-criteria 
          (map (lambda (x) (match x [(list _ owner pkg _ maj min)
                                     (list owner pkg maj min)]))
               normal-packages)
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
           (lambda (x) (match x [(list dir owner pkg _ maj min)
                                 (list owner pkg maj min (path->string dir))]))
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
     (sort (current-linkage) string<? #:key car)))
  
  (define (add-hard-link-cmd ownerstr pkgstr majstr minstr pathstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [path (string->path pathstr)])
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (add-hard-link ownerstr pkgstr maj min path)))
  
  (define (remove-hard-link-cmd ownerstr pkgstr majstr minstr quiet?)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)])
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (remove-hard-link ownerstr pkgstr maj min #:quiet? quiet?)))
      
  (define (get-download-url ownerstr pkgstr majstr minstr)
    (let ([fps (params->full-pkg-spec ownerstr pkgstr majstr minstr)])
      (printf "~a\n" (url->string (pkg->download-url fps)))))
  
  (define (do-unpack plt-file target)
    (unless (file-exists? plt-file)
      (fail (format "The specified file (~a) does not exist" plt-file))) 
    (let ([file (simple-form-path plt-file)])
      (unpack-planet-archive file target)))
  
  (define (do-structure plt-file)
    (unless (file-exists? plt-file)
      (fail (format "The specified file (~a) does not exist" plt-file))) 
    (let ([file (simple-form-path plt-file)])
      (display-plt-file-structure file)))
  
  (define (do-display plt-file file-to-print)
    (unless (file-exists? plt-file)
      (fail (format "The specified file (~a) does not exist" plt-file))) 
    (let ([file (simple-form-path plt-file)])
      (display-plt-archived-file file file-to-print)))
  
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
