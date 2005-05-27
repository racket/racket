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
           (prefix list: (lib "list.ss"))
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
      
      ;; unimplemented so far:
      #;(("-u" "--unlink")
         module
         "Remove all linkage the given module has, forcing it to upgrade"
         ...)))

    (for-each (lambda (f) (f)) actions))
  
  ;; ============================================================
  ;; FEATURE IMPLEMENTATIONS
  
  (define (fail s) (raise (make-exn:fail s (current-continuation-marks))))
  
  (define (download/install owner pkg majstr minstr)
    (let* ([maj (read-from-string majstr)]
           [min (read-from-string minstr)]
           [full-pkg-spec (pkg-spec->full-pkg-spec (list owner pkg maj min) #f)])
      (when (get-package-from-cache full-pkg-spec)
        (fail "No package installed (cache already contains a matching package)"))
      (unless (get-package-from-server full-pkg-spec)
        (fail "Could not find matching package"))))
  
  (define (install-plt-file filestr owner majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (unless (file-exists? filestr) (fail (format "File does not exist: ~a" filestr)))
      (let* ([file (normalize-path filestr)]
             [name (let-values ([(base name dir?) (split-path file)]) (path->string name))]
             [spec (list owner name maj min)]
             [fullspec (pkg-spec->full-pkg-spec spec #f)])
        (unless spec (fail "invalid spec: ~a" spec))
        (install-pkg fullspec file maj min))))
  
  (define (do-archive p)
    (unless (directory-exists? p)
      (fail (format "No such directory: ~a" p)))
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
     (list:quicksort 
      (current-linkage)
      (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))
  
  
  
  
  ;; ------------------------------------------------------------
  ;; Utility
    
  (define (sort-by-criteria l . criteria)
    (list:quicksort
     l
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
