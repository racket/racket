#lang racket/base
#| planet-shared.rkt -- shared client/server utility functions

Various common pieces of code that both the client and server need to access
==========================================================================================
|#
  
  (require (only-in racket/path path-only)
           mzlib/port
           racket/file
           setup/getinfo
           (prefix-in srfi1: srfi/1)
           "../config.rkt"
           "data.rkt")
  
  (provide (all-from-out "data.rkt")
           (struct-out exn:fail:filesystem:no-directory)
           (struct-out mz-version)
           (struct-out branch)
           (struct-out star)
           try-make-directory*
           language-version->repository
           version->description
           legal-language?
           lookup-package
           lookup-package-by-keys
           empty-table
           get-min-core-version
           points-to?
           row->package
           
           add-hard-link!
           filter-link-table!
           get-hard-link-table
           
           update-element
           update/create-element
           first-n-list-selectors
           make-assoc-table-row
           string->mz-version
           version<=
           pkg<
           pkg>
           pkg=
           compatible-version?
           get-best-match
           get-installed-package
           make-cutoff-port
           write-line
           for-each/n
           nat?
           read-n-chars-to-file
           copy-n-chars
           repeat-forever
           build-hash-table
           categorize
           drop-last
           read-all
           wrap
           planet-logging-to-stdout
           planet-log
           with-logging
           pkg->info
           directory->tree
           filter-tree-by-pattern
           tree-apply
           tree->list
           repository-tree
           
           assoc-table-row->name
           assoc-table-row->path
           assoc-table-row->maj
           assoc-table-row->min
           assoc-table-row->dir
           assoc-table-row->required-version
           assoc-table-row->type
           
           check/take-installation-lock
           dir->successful-installation-file
           dir->unpacked-file
           dir->metadata-files
           
           powerful-security-guard
           with-powerful-security-guard)
  
  (define powerful-security-guard (make-parameter #f))
  (define-syntax-rule 
    (with-powerful-security-guard e1 e2 ...)
    (with-powerful-security-guard/proc (λ () e1 e2 ...)))
  (define (with-powerful-security-guard/proc t)
    (parameterize ([current-security-guard (or (powerful-security-guard) (current-security-guard))])
      (t)))
  
  ; ==========================================================================================
  ; CACHE LOGIC
  ; Handles checking the cache for an appropriate module
  ; ==========================================================================================
  
  ; language-version->repository : string -> string | #f
  ; finds the appropriate language version for the given repository
  (define (language-version->repository ver)
    (cond
      [(regexp-match #rx"^20.+" ver) "207.1"]
      [(regexp-match #rx"(^3..+)|(^29.+)" ver) "300"]
      [else #f]))
  
  (define (version->description ver)
    (cond
      [(string=? ver "207.1") "20x"]
      [(string=? ver "300") "299.x-3xx"]
      [else (error 'version->description "Expected one of 207.1 and 300, got ~a" ver)]))
  
  (define (legal-language? l)
    (and (language-version->repository l) #t))
  
  ; lookup-package : FULL-PKG-SPEC [path (optional)] -> PKG | #f
  ; returns the directory pointing to the appropriate package in the cache, the user's hardlink table,
  ; or #f if the given package isn't in the cache or the hardlink table
  (define (lookup-package pkg [dir (CACHE-DIR)] #:to-check [to-check #f])
    (define at (build-assoc-table pkg dir to-check))
    (get-best-match at pkg))
  
  ; build-assoc-table : FULL-PKG-SPEC path -> assoc-table
  ; returns a version-number -> directory association table for the given package
  (define (build-assoc-table pkg dir to-check) 
    (append 
     (pkg->assoc-table pkg dir to-check)
     (hard-links pkg)))
  
  
  ;; lookup-package-by-keys : string string nat nat nat -> (list path string string (listof string) nat nat) | #f
  ;; looks up and returns a list representation of the package named by the given owner,
  ;; package name, major and (exact) minor version.
  ;; this function is intended to be useful for setup-plt and other applications that need to know where planet
  ;; packages live
  (define (lookup-package-by-keys owner name maj min-lo min-hi)
    (let ([result
           (lookup-package
            (make-pkg-spec
             name
             maj
             min-lo
             min-hi
             (list owner)
             #f
             (version)))])
      (if result
          (list (pkg-path result)
                (car (pkg-route result))
                (pkg-name result)
                (cdr (pkg-route result))
                (pkg-maj result)
                (pkg-min result))
          #f)))
     
  
  ;; assoc-table ::= (listof (list n n path))
  (define empty-table '())
  
  ;; get-min-core-version : path -> string | #f
  (define (get-min-core-version p)
    (let ((info (with-handlers ([exn:fail? (lambda (e) #f)])
                  (get-info/full p))))
      (if info
          (let ((core (info 'required-core-version (lambda () #f))))
            (if (and core (string? core))
                core
                #f))
          #f)))
  
  ; pkg->assoc-table : FULL-PKG-SPEC path boolean? -> assoc-table
  ; returns the on-disk packages for the given planet package in the
  ; on-disk table rooted at the given directory
  (define (pkg->assoc-table pkg dir to-check)
    (define path (build-path (apply build-path dir (pkg-spec-path pkg)) (pkg-spec-name pkg)))
    
    (define (tree-stuff->row-or-false p majs mins)
      (let ((maj (string->number majs))
            (min (string->number mins)))
        (if (and (path? p) maj min)
            (let* ((the-path         (build-path path majs mins))
                   (min-core-version (get-min-core-version the-path)))
              (and (cond
                     [(eq? to-check 'success)
                      (if (member the-path (held-locks))
                          ;; this means we're in the process of installing this package
                          ;; and the files should be already there in the filesystem
                          ;; so we count that as just having to check if they are unpacked
                          
                          (file-exists? (dir->unpacked-file the-path))
                          (file-exists? (dir->successful-installation-file the-path)))]
                     [(eq? to-check 'unpacked)
                      (file-exists? (dir->unpacked-file the-path))]
                     [else #t])
                   (make-assoc-table-row 
                    (pkg-spec-name pkg) 
                    (pkg-spec-path pkg)
                    maj min
                    the-path
                    min-core-version
                    'normal)))
            #f)))
    (if (directory-exists? path)
        (filter
         (λ (x) x)
         (tree-apply
          tree-stuff->row-or-false
          (directory->tree path (λ (x) #t) 2 (λ (x) x))))
        empty-table))
  
  ; the link table format:
  ; (listof (list string[name] (listof string[path]) num num bytes[directory] (union string[racket version] #f))
  
  ; hard-links : FULL-PKG-SPEC -> (listof assoc-table-row)
  (define (hard-links pkg)
    (filter
     (λ (row)
       (and (equal? (assoc-table-row->name row) (pkg-spec-name pkg))
            (equal? (assoc-table-row->path row) (pkg-spec-path pkg))))
     (get-hard-link-table)))

  ;; verify-well-formed-hard-link-parameter! : -> void
  ;; pitches a fit if the hard link table parameter isn't set right
  (define (verify-well-formed-hard-link-parameter!)
    (define hlf (HARD-LINK-FILE))
    (unless (and (absolute-path? hlf) (path-only hlf))
      (raise (make-exn:fail:contract
              (format
               "The HARD-LINK-FILE setting must be an absolute path name specifying a file; given ~s"
               hlf)
              (current-continuation-marks)))))

  ;; get-hard-link-table/internal : -> assoc-table
  (define (get-hard-link-table/internal)
    (verify-well-formed-hard-link-parameter!)
    (with-powerful-security-guard
     (if (file-exists? (HARD-LINK-FILE))
         (map (lambda (item) (update/create-element 6 (λ (_) 'development-link) (update-element 4 bytes->path item)))
              (with-input-from-file (HARD-LINK-FILE) read-all))
         '())))
  
  (define (with-hard-link-lock t)
    (with-powerful-security-guard
     (let-values ([(base name dir) (split-path (HARD-LINK-FILE))])
       (try-make-directory* base))
     (call-with-file-lock/timeout
      (HARD-LINK-FILE)
      'exclusive
      t
      (λ () 
        (error 'planet/planet-shared.rkt "unable to obtain lock on ~s" (HARD-LINK-FILE))))))
  
  (define (get-hard-link-table)
    ;; we can only call with-hard-link-lock when the directory containing
    ;; (HARD-LINK-FILE) exists
    (if (with-powerful-security-guard (file-exists? (HARD-LINK-FILE)))
        (with-hard-link-lock
         (λ ()
           (get-hard-link-table/internal)))
        '()))
  
  ;; row-for-package? : row string (listof string) num num -> boolean
  ;; determines if the row associates the given package with a dir
  (define (points-to? row name path maj min)
    (and (equal? name (assoc-table-row->name row))
         (equal? path (assoc-table-row->path row))
         (equal? maj  (assoc-table-row->maj row))
         (equal? min  (assoc-table-row->min row))))
  
  ;; row->package : assoc-table-row -> PKG | #f
  (define (row->package row)
    (get-installed-package 
     (car (assoc-table-row->path row)) ;; owner
     (assoc-table-row->name row)
     (assoc-table-row->maj row)
     (assoc-table-row->min row)))
  
  ;; save-hard-link-table : assoc-table -> void
  ;; saves the given table, overwriting any file that might be there
  ;; assumes that the lock on the HARD-LINK table file has been acquired
  (define (save-hard-link-table table)
    (verify-well-formed-hard-link-parameter!)
    (with-powerful-security-guard
     (with-output-to-file (HARD-LINK-FILE) #:exists 'truncate
       (lambda ()
         (display "")
         (for-each 
          (lambda (row)
            (write (update-element 4 path->bytes row))
            (newline))
          table)))))
  
  ;; add-hard-link! string (listof string) num num path -> void
  ;; adds the given hard link, clearing any previous ones already in place
  ;; for the same package
  (define (add-hard-link! name path maj min dir)
    (with-hard-link-lock
     (λ ()
       (let ([complete-dir (path->complete-path dir)])
         (let* ([original-table (get-hard-link-table/internal)]
                [new-table (cons
                            (make-assoc-table-row name path maj min complete-dir #f 'development-link)
                            (filter
                             (lambda (row) (not (points-to? row name path maj min)))
                             original-table))])
           (save-hard-link-table new-table))))))
  
  ;; filter-link-table! : (row -> boolean) (row -> any/c) -> void
  ;; removes all rows from the hard link table that don't match the given predicate.
  ;; also updates auxiliary datastructures that might have dangling pointers to
  ;; the removed links
  (define (filter-link-table! f on-delete)
    (define out-links
      (with-hard-link-lock
       (λ ()
         (let-values ([(in-links out-links) (srfi1:partition f (get-hard-link-table/internal))])
           (save-hard-link-table in-links)
           out-links))))
      (for-each on-delete out-links))
  
  ;; update-element : number (x -> y) (listof any [x in position number]) -> (listof any [y in position number])
  (define (update-element n f l)
    (cond
      [(null? l) (error 'update-element "Index too large")]
      [(zero? n) (cons (f (car l)) (cdr l))]
      [else (cons (car l) (update-element (sub1 n) f (cdr l)))]))
  
  (define (update/create-element n f l)
    (cond
      [(and (null? l) (zero? n))
       (list (f #f))]
      [(and (null? l) (not (zero? n)))
       (error 'update/create-element "Index too large")]
      [(and (not (null? l)) (zero? n))
       (cons (f (car l)) (cdr l))]
      [else (cons (car l) (update/create-element (sub1 n) f (cdr l)))]))
       
  ;; first-n-list-selectors : number -> (values (listof x -> x) ...)
  ;; returns n list selectors for the first n elements of a list
  ;; (useful for defining meaningful names to list-structured data)
  (define (first-n-list-selectors n)
    (apply values (build-list n (lambda (m) (lambda (row) (list-ref row m))))))
  
  ;; assoc-table-row->{name,path,maj,min,dir,required-version} 
  ;; : assoc-table-row ->
  ;; {string,(listof string),num,num,path,string|#f}
  ;; retrieve the {package name, "package path", major version, minor version, directory, required core version}
  ;; of the given row
  (define-values (assoc-table-row->name
                  assoc-table-row->path
                  assoc-table-row->maj
                  assoc-table-row->min
                  assoc-table-row->dir
                  assoc-table-row->required-version
                  assoc-table-row->type)
    (first-n-list-selectors 7))
  
  (define (make-assoc-table-row name path maj min dir required-version type)
    (list name path maj min dir required-version type))
  
  (define-struct mz-version (major minor) #:inspector #f)
  
  
  ;; string->mz-version : string -> mz-version | #f
  ;; Converts a string into mz-version.  We need to account
  ;; for the change in numbering style from the 372 era to the 4.0 era.
  (define (string->mz-version str)
    (define (minor+maint-chunks->minor chunks)
      (+ (* (string->number (car chunks)) 1000)
         (if (> (length chunks) 1)
             (string->number (cadr chunks))
             0)))
    
    (cond
      ;; Old style numbering with three digits in front.  The first digit
      ;; goes up to three.
      [(regexp-match #rx"^([0-3][0-9][0-9])\\.?([.0-9]*)$" str)
       =>
       (lambda (ver)
         (let ([major (string->number (list-ref ver 1))])
           (cond
             [(= (string-length (list-ref ver 2)) 0)
              (make-mz-version major 0)]
             [else
              (let* ([minor+maint (regexp-split #rx"\\." (list-ref ver 2))]
                     [minor (minor+maint-chunks->minor minor+maint)])
                (make-mz-version major minor))])))]
      ;; New style numbering
      [(regexp-match #rx"^([0-9]+)(\\.([.0-9]+))?$" str)
       =>
       (lambda (ver)
         (cond [(list-ref ver 3)
                (let* ([chunks (regexp-split #rx"\\." (list-ref ver 3))])
                  (and (andmap (λ (x) (not (equal? x ""))) chunks)
                       (make-mz-version (+ (* (string->number (list-ref ver 1))
                                              100)
                                           (if (> (length chunks) 0)
                                               (begin
                                                 (string->number (car chunks)))
                                               0))
                                        (if (> (length (cdr chunks)) 0)
                                            (minor+maint-chunks->minor (cdr chunks))
                                            0))))]
               [else
                (make-mz-version (* (string->number (list-ref ver 1))
                                    100)
                                 0)]))]
      [else #f]))
  
  
  
  ;; version<= : mz-version mz-version -> boolean
  ;; determines if a is the version string of an earlier racket release than b
  ;;   [n.b. this relies on a guarantee from Matthew that racket version
  ;;    x1.y1 is older than version x2.y2 iff x1<x2 or x1=x2 and y1<y2]
  (define (version<= a b)
    (or (<= (mz-version-major a) (mz-version-major b))
        (and (= (mz-version-major a) (mz-version-major b))
             (<= (mz-version-minor a) (mz-version-minor b)))))
  
  ;; pkg< : pkg pkg -> boolean
  ;; determines if a is an earlier version than b
  ;; [only sensical if a and b are versions of the same package]
  (define (pkg< a b)
    (or (< (pkg-maj a) (pkg-maj b))
        (and (= (pkg-maj a) (pkg-maj b))
             (< (pkg-min a) (pkg-min b)))))
  
  (define (pkg> a b) 
    (pkg< b a))
  (define (pkg= a b) 
    (not (or (pkg< a b) (pkg> a b))))
  
  ;; compatible-version? : assoc-table-row FULL-PKG-SPEC -> boolean
  ;; determines if the given package constrint verstr can support the given package
  (define (compatible-version? row spec)
    (let ((required-version (assoc-table-row->required-version row)))
      (or (not required-version)
          (let ((required (string->mz-version required-version))
                (provided (string->mz-version (pkg-spec-core-version spec))))
            (or (not required) 
                (not provided)
                (version<= required provided))))))
  
  ; get-best-match : assoc-table FULL-PKG-SPEC -> PKG | #f
  ; return the best on-disk match for the given package spec
  (define (get-best-match table spec)
    (if (null? table)
        #f
        (let* ((target-maj 
                (or (pkg-spec-maj spec)
                    (apply max (map assoc-table-row->maj table))))
               (lo (pkg-spec-minor-lo spec))
               (hi (pkg-spec-minor-hi spec))
               (matches
                (filter 
                 (λ (x) 
                   (let ((n (assoc-table-row->min x)))
                     (and
                      (equal? target-maj (assoc-table-row->maj x))
                      (or (not lo) (>= n lo))
                      (or (not hi) (<= n hi))
                      (compatible-version? x spec))))
                 table)))
          (if (null? matches)
              #f
              (let ((best-row
                     (car 
                      (sort
                       matches
                       (λ (a b) (> (assoc-table-row->min a) (assoc-table-row->min b)))))))
                (make-pkg
                 (pkg-spec-name spec)
                 (pkg-spec-path spec)
                 (assoc-table-row->maj best-row)
                 (assoc-table-row->min best-row)
                 (assoc-table-row->dir best-row)
                 (assoc-table-row->type best-row)))))))
  
  
  ;; get-installed-package : string string nat nat -> PKG | #f
  ;; gets the package associated with this package specification, if any
  (define (get-installed-package owner name maj min)
    (lookup-package (make-pkg-spec name maj min min (list owner) #f (version))))
  
  
  ; ==========================================================================================
  ; UTILITY
  ; Miscellaneous utility functions
  ; ==========================================================================================
  
  ; make-cutoff-port : input-port nat [nat -> tst] -> input-port
  ; makes a new input port that reads the first n characters from the given port, then
  ; returns eof. If n characters are not available from the given input port, calls
  ; the given function and then returns eof
  (define make-cutoff-port
    (lambda (ip n [underflow-fn void])
      (let ((to-read n))
        (make-input-port 
         'cutoff-port
         
         (lambda (bytestr)
           (cond
             [(= to-read 0) eof]
             [else
              (let ((bytes-read (read-bytes-avail! bytestr ip 0 (min n (bytes-length bytestr)))))
                (if (eof-object? bytes-read)
                    (begin
                      (underflow-fn (- to-read bytes-read))
                      (set! to-read 0)
                      eof)
                    (begin
                      (set! to-read (- to-read bytes-read))
                      bytes-read)))]))
         #f
         void))))
  
  ; write-line : X output-port -> void
  ; writes the given value followed by a newline to the given port
  (define (write-line obj p)
    (write obj p)
    (newline p))
  
  ; for-each/n (X Nat -> Y) (listof X) -> void
  ; calls the input function on each element of the input list in order,
  ; also providing the element's zero-based index in the list
  (define (for-each/n f l)
    (let loop ((l l) (n 0))
      (cond
        [(null? l) (void)]
        [else
         (f (car l) n)
         (loop (cdr l) (add1 n))])))
  
  ; nat? : TST -> bool
  ; determines if the given value is a natural number
  (define (nat? obj) (and (integer? obj) (>= obj 0)))
  
  ; read-n-chars-to-file : Nat input-port string[filename] -> void
  ; copies exactly n chars to the given file from the given port. Raises an exception
  ; if the given number of characters are not available.
  (define (read-n-chars-to-file n ip file)
    (let ((op (open-output-file file #:exists 'truncate)))
      (copy-n-chars n ip op)
      (close-output-port op)))
  
  ; copy-n-chars : Nat input-port output-port -> void
  ; copies exactly n characters from the input to the output. Raises an exception
  ; if this is not possible.
  (define (copy-n-chars n ip op)
    (let ((cport (make-cutoff-port ip 
                                   n
                                   (lambda (m)
                                     (raise 
                                      (make-exn:fail:read:eof
                                       (format "Not enough chars on input (expected ~a, got ~a)" 
                                               n 
                                               m)
                                       (current-continuation-marks)
                                       ip))))))
      (copy-port cport op)))
  
  ; repeat-forever : (-> void) -> [diverges]
  ; repeatedly invokes the given thunk forever
  (define (repeat-forever thunk) (let loop () (thunk) (loop)))
  
  ; build-hash-table : listof (list X Y) -> equal-hash-table[X -> Y]
  ; builds a new hash-table mapping all given X's to their appropriate Y values
  (define (build-hash-table asl)
    (let ((ht (make-hash)))
      (for-each (lambda (item) (hash-set! ht (car item) (cadr item))) asl)
      ht))
  
  ; categorize : (X -> Y) (listof X) -> (listof (cons Y (listof X)))
  ; sorts the l into categories given by f
  (define (categorize f l)
    (let ((ht (make-hash)))
      (for-each 
       (lambda (i)
         (let ((key (f i)))
           (hash-set! ht key (cons i (hash-ref ht key (lambda () '()))))))
       l)
      (hash-map ht cons)))
  
  (define (drop-last l) (reverse (cdr (reverse l))))
  
  ;; note: this can be done faster by reading a copy-port'ed port with
  ;; ( and ) tacked around it
  (define read-all
    (case-lambda
      [() (read-all (current-input-port))]
      [(ip)
       (let ((sexpr (read ip)))
         (cond
           [(eof-object? sexpr) '()]
           [else (cons sexpr (read-all ip))]))]))
  
  (define (wrap x) (begin (write x) (newline) x))
  
  (define planet-logging-to-stdout (make-parameter #f))
  
  (define planet-log 
    (let ([planet-logger (make-logger 'PLaneT (current-logger))])
      (λ (str . fmt)
        (define formatted-str (apply format str fmt))
        (parameterize ([current-logger planet-logger])
          (log-info formatted-str)
          (when (planet-logging-to-stdout)
            (fprintf (current-output-port) "PLaneT: ~a" formatted-str)
            (newline (current-output-port)))))))
  
  ;; note that this function assumes that 'f' prints line-by-line
  ;; output, so that it can be easily logged.
  (define (with-logging logfile f)
    (let-values ([(in out) (make-pipe)])
      (thread
       (λ () 
         (let ([outport
                (and logfile
                     (with-handlers ((exn:fail:filesystem? (lambda (e) #f)))
                       (open-output-file logfile #:exists 'append)))])
           (let loop ()
             (let ([l (read-line in)])
               (cond
                 [(eof-object? l)
                  (close-input-port in) 
                  (when outport (close-output-port outport))]
                 [else
                  (when outport 
                    (display l outport)
                    (newline outport)
                    (flush-output outport))
                  (planet-log "~a" l)
                  (loop)]))))))
      (begin0
        (parameterize ([current-output-port out])
          (f))
        (close-output-port out))))
  
  ;; pkg->info : PKG -> (symbol (-> TST) -> TST)
  ;; get an info.rkt thunk for the given package
  (define (pkg->info p)
    (or
     (with-handlers ([exn:fail? (lambda (e) #f)])
       (get-info/full (pkg-path p)))
     (lambda (s thunk) (thunk))))
  
  ;; ============================================================
  ;; TREE STUFF
  ;; ============================================================
  
  ;; tree[X] ::= (make-branch X (listof tree[X])
  (define-struct branch (node children) #:transparent)
  
  (define-struct (exn:fail:filesystem:no-directory exn:fail:filesystem) (dir))
  
  ;; directory->tree : directory (string -> bool) [nat | bool] [path->X] -> tree[X] | #f
  (define (directory->tree directory valid-dir? [max-depth #f] [path->x path->string])
    (unless (directory-exists? directory)
      (raise (make-exn:fail:filesystem:no-directory 
              "Directory ~s does not exist"
              (current-continuation-marks)
              directory)))
    (let-values ([(path name _) (split-path directory)])
      (let* ((files (directory-list directory))
             (files (map (lambda (d) (build-path directory d)) files))
             (files (filter (lambda (d) (and (directory-exists? d) (valid-dir? d))) files)))
        (make-branch 
         (path->x name)
         ;; NOTE: the above line should not use path->string. I don't have time to track this down though
         (if (equal? max-depth 0) 
             '()
             (let ((next-depth (if max-depth (sub1 max-depth) #f)))
               (map (lambda (d) (directory->tree d valid-dir? next-depth)) files)))))))
  
  ;; filter-pattern : (listof pattern-term)
  ;; pattern-term   : (x -> y) | (make-star (tst -> bool) (x -> y))
  (define-struct star (pred fun))
  
  ;; filter-tree-by-pattern : tree[x] filter-pattern -> tree[y]
  ;; constraint: depth of the tree <= length of the list
  ;; converts the tree by applying to each depth the function at that position in the list
  (define (filter-tree-by-pattern tree pattern)
    (cond
      [(null? pattern) (error 'filter-tree-by-pattern "Tree too deep: ~e" tree)]
      [(star? (car pattern))
       (if (star-pred (car pattern))
           (make-branch
            (star-fun (branch-node tree))
            (map (lambda (x) (filter-tree-by-pattern x pattern))
                 (branch-children tree)))
           (filter-tree-by-pattern tree (cdr pattern)))]
      [else
       (make-branch ((car pattern) (branch-node tree))
                    (map 
                     (lambda (x) (filter-tree-by-pattern x (cdr pattern)))
                     (branch-children tree)))]))
  
  ;; sexp-tree[x] ::= (cons x (listof sexp-tree[x]))
  
  ;; tree-apply : (... -> tst) tree -> listof tst
  ;; applies f to every path from root to leaf and
  ;; accumulates all results in a list
  (define (tree-apply f t [depth 0])
    (let loop ((t t)
               (priors '())
               (curr-depth 0))
      (cond
        [(null? (branch-children t))
         (if (> curr-depth depth)
             (let ([args (reverse (cons (branch-node t) priors))])
               (if (procedure-arity-includes? f (length args))
                   (list (apply f args))
                   '()))
             '())]
        [else
         (let ((args (cons (branch-node t) priors)))
           (apply append
                  (map (λ (x) (loop x args (add1 curr-depth)))
                       (branch-children t))))])))
  
  ;; tree->list : tree[x] -> sexp-tree[x]
  (define (tree->list tree)
    (cons (branch-node tree) (map tree->list (branch-children tree))))
  
  

  (define (repository-tree)
    (define (id x) x)
    (filter-tree-by-pattern
     (directory->tree
      (CACHE-DIR)
      (lambda (x)
        (not (regexp-match? #rx"/(?:[.]git.*|[.]svn|CVS)$" (path->string x))))
      4)
     (list id id id string->number string->number)))

;; try-make-directory* : path[directory] -> void
;; tries multiple times to make the directory 'dir'
;; we only expect the second (or later) attempt to succeed
;; when two calls to try-make-directory* happen in parallel
;; (in separate places); this is here to avoid having to use
;; a lock
(define (try-make-directory* dir)
  (let loop ([n 10])
    (cond
      [(zero? n)
       (make-directory* dir)]
      [else
       (with-handlers ((exn:fail:filesystem? (λ (x) (loop (- n 1)))))
         (make-directory* dir))])))





;                                                                                              
;                                                                                              
;                                                                                              
;                                                                                              
;  ;;;                  ;          ;;; ;;;     ;;;                 ;;;     ;;;                 
;                     ;;;          ;;; ;;;     ;;;                 ;;;                         
;  ;;; ;;; ;;   ;;;;  ;;;;  ;;;;;  ;;; ;;;     ;;;   ;;;     ;;;   ;;;  ;;;;;; ;;; ;;   ;; ;;; 
;  ;;; ;;;;;;; ;;; ;; ;;;; ;;;;;;; ;;; ;;;     ;;;  ;;;;;   ;;;;;  ;;; ;;; ;;; ;;;;;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;;    ;;;  ;;  ;;; ;;; ;;;     ;;; ;;; ;;; ;;;  ;; ;;;;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;;  ;;;;  ;;;    ;;;;; ;;; ;;;     ;;; ;;; ;;; ;;;     ;;;;;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;;    ;;; ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;; ;;;  ;; ;;;;;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;; ;;; ;;;; ;;; ;;; ;;; ;;;     ;;;  ;;;;;   ;;;;;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; 
;  ;;; ;;; ;;;  ;;;;   ;;;  ;;;;;; ;;; ;;;     ;;;   ;;;     ;;;   ;;;  ;;;;;; ;;; ;;;  ;; ;;; 
;                                                                                          ;;; 
;                                                                                      ;;;;;;  
;                                                                                              
;                                                                                              



;; check/take-installation-lock : path (-> void) -> (or/c port #f)
;; if this function returns #t, then it successfully
;;   optained the installation lock.
;; if it returns #f, then we tried to grab the lock, but someone
;;   else already had it, so we waited until that installation finished 
(define (check/take-installation-lock dir do-installation)
  (define lf (dir->lock-file dir))
  ;; make sure the lock file exists
  (with-handlers ((exn:fail:filesystem:exists? void))
    (call-with-output-file lf void))
  (define p (with-powerful-security-guard (open-output-file lf #:exists 'truncate)))
  (cond
    [(port-try-file-lock? p 'exclusive)
     ;; we got the lock; keep the file open
     (parameterize ([held-locks (cons dir (held-locks))])
       (dynamic-wind
        (λ () (void))
        (λ () (do-installation))
        (λ () (close-output-port p))))]
    [else
     ;; we didn't get the lock (and didn't alreayd have it); poll for the SUCCESS FILE
     (planet-log "waiting for someone else to finish installation in ~s" dir)
     (let loop ()
       (cond
         [(file-exists? (dir->successful-installation-file dir))
          (planet-log "continuing; someone else finished installation in ~s" dir)
          #f]
         [else
          (sleep 2)
          (loop)]))]))

(define held-locks (make-parameter '()))

(define (dir->successful-installation-file dir) (dir->something-file dir #".SUCCESS"))
(define (dir->lock-file dir) (dir->something-file dir #".LOCK"))
(define (dir->unpacked-file dir) (dir->something-file dir #".UNPACKED"))

(define (dir->something-file dir something)
  (define-values (base name dir?) (split-path dir))
  (build-path base (bytes->path (bytes-append (path->bytes name) something))))

(define (dir->metadata-files dir)
  (list (dir->lock-file dir)
        (dir->unpacked-file dir)
        (dir->successful-installation-file dir)))
