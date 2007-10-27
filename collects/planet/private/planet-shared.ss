#| planet-shared.ss -- shared client/server utility functions

Various common pieces of code that both the client and server need to access
==========================================================================================
|#

(module planet-shared mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "file.ss")
           (lib "getinfo.ss" "setup")
           (prefix srfi1: (lib "1.ss" "srfi"))
           "../config.ss")
  
  (provide (all-defined))
 
  
  ; ==========================================================================================
  ; DATA
  ; defines common data used by the PLaneT system
  ; ==========================================================================================
  
  ; exn:i/o:protocol: exception indicating that a protocol error occured
  (define-struct (exn:i/o:protocol exn:fail:network) ())

  ; FULL-PKG-SPEC : struct pkg-spec
  (define-struct pkg-spec 
    (name           ; string
     maj            ; (Nat | #f)
     minor-lo       ; (Nat | #f)
     minor-hi       ; (Nat | #f)
     path           ; (listof string)
     stx            ; (syntax | #f)
     core-version   ; string
     )
    (make-inspector))
  ; PKG : string (listof string) Nat Nat path ORIGIN
  (define-struct pkg (name route maj min path origin))
  ; UNINSTALLED-PKG : path FULL-PKG-SPEC Nat Nat
  (define-struct uninstalled-pkg (path spec maj min))
  ; PKG-PROMISE : PKG | UNINSTALLED-PKG
  ; ORIGIN : 'normal | 'development-link
  
  (define (pkg-promise? p) (or (pkg? p) (uninstalled-pkg? p)))

  (define (normally-installed-pkg? p)
    (eq? (pkg-origin p) 'normal))
  
  (define (development-link-pkg? p)
    (eq? (pkg-origin p) 'development-link))
  
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
  (define lookup-package
    (case-lambda 
      [(pkg) (lookup-package pkg (CACHE-DIR))]
      [(pkg dir)
       (let* ((at (build-assoc-table pkg dir)))
         (get-best-match at pkg))]))

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
     
  
  ; build-assoc-table : FULL-PKG-SPEC path -> assoc-table
  ; returns a version-number -> directory association table for the given package
  (define (build-assoc-table pkg dir) 
    (add-to-table 
     (pkg->assoc-table pkg dir)
     (hard-links pkg)))
  
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
  
  ; pkg->assoc-table : FULL-PKG-SPEC path -> assoc-table
  ; returns the on-disk packages for the given planet package in the
  ; on-disk table rooted at the given directory
  (define (pkg->assoc-table pkg dir)
    (define path (build-path (apply build-path dir (pkg-spec-path pkg)) (pkg-spec-name pkg)))
    
    (define (tree-stuff->row-or-false p majs mins)
      (let ((maj (string->number majs))
            (min (string->number mins)))
        (if (and (path? p) maj min)
            (let* ((the-path         (build-path path majs mins))
                   (min-core-version (get-min-core-version the-path)))
              (make-assoc-table-row 
               (pkg-spec-name pkg) 
               (pkg-spec-path pkg)
               maj min
               the-path
               min-core-version
               'normal))
            #f)))
    
    (if (directory-exists? path)
        (filter
         (λ (x) x)
         (tree-apply
          tree-stuff->row-or-false
          (directory->tree path (λ (x) #t) 2 (λ (x) x))))
        empty-table))
  
  ; the link table format:
  ; (listof (list string[name] (listof string[path]) num num bytes[directory] (union string[mzscheme version] #f))
  
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
    (unless (and (absolute-path? (HARD-LINK-FILE)) (path-only (HARD-LINK-FILE)))
      (raise (make-exn:fail:contract
              (format
               "The HARD-LINK-FILE setting must be an absolute path name specifying a file; given ~s"
               (HARD-LINK-FILE))
              (current-continuation-marks)))))

  ;; get-hard-link-table : -> assoc-table
  (define (get-hard-link-table)
    (verify-well-formed-hard-link-parameter!)
    (if (file-exists? (HARD-LINK-FILE))
        (map (lambda (item) (update/create-element 6 (λ (_) 'development-link) (update-element 4 bytes->path item)))
             (with-input-from-file (HARD-LINK-FILE) read-all))
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
  (define (save-hard-link-table table)
    (verify-well-formed-hard-link-parameter!)
    (with-output-to-file (HARD-LINK-FILE)
      (lambda ()
        (display "")
        (for-each 
         (lambda (row)
           (write (update-element 4 path->bytes row))
           (newline))
         table))
      'truncate))
  
  ;; add-hard-link! string (listof string) num num path -> void
  ;; adds the given hard link, clearing any previous ones already in place
  ;; for the same package
  (define (add-hard-link! name path maj min dir)
    (let ([complete-dir (path->complete-path dir)])
      (let* ([original-table (get-hard-link-table)]
             [new-table (cons
                         (make-assoc-table-row name path maj min complete-dir #f 'development-link)
                         (filter
                          (lambda (row) (not (points-to? row name path maj min)))
                          original-table))])
        (save-hard-link-table new-table))))
  
  ;; filter-link-table! : (row -> boolean) -> void
  ;; removes all rows from the hard link table that don't match the given predicate.
  ;; also updates auxiliary datastructures that might have dangling pointers to
  ;; the removed links
  (define (filter-link-table! f on-delete)
    (let-values ([(in-links out-links) (srfi1:partition f (get-hard-link-table))])
      (for-each on-delete out-links)
      (save-hard-link-table in-links)))
  
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
       
  
  ; add-to-table assoc-table (listof assoc-table-row) -> assoc-table
  (define add-to-table append) 
  
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
  
  (define-struct mz-version (major minor))
  
  ;; string->mz-version : string -> mz-version | #f
  (define (string->mz-version str)
    (let ((ver (regexp-match #rx"^([0-9]+)(\\.([0-9]+))?$" str)))
      (if ver
          (make-mz-version 
           (string->number (list-ref ver 1))
           (if (list-ref ver 3)
               (string->number (list-ref ver 3))
               0))
          #f)))
  
  ;; version<= : mz-version mz-version -> boolean
  ;; determines if a is the version string of an earlier mzscheme release than b
  ;;   [n.b. this relies on a guarantee from Matthew that mzscheme version
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
                      (quicksort
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
    (opt-lambda (ip n [underflow-fn void])
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
  ; determines if the given scheme value is a natural number
  (define (nat? obj) (and (integer? obj) (>= obj 0)))
  
  ; read-n-chars-to-file : Nat input-port string[filename] -> void
  ; copies exactly n chars to the given file from the given port. Raises an exception
  ; if the given number of characters are not available.
  (define (read-n-chars-to-file n ip file)
    (let ((op (open-output-file file 'truncate)))
      (copy-n-chars n ip op)
      (close-output-port op)))
  
  ; copy-n-chars : Nat input-port output-port -> void
  ; copies exactly n characters from the input to the output. Raises an exception
  ; if this is not possible.
  (define (copy-n-chars n ip op)
    (let ((cport (make-cutoff-port ip 
                                   n
                                   (lambda ()
                                     (raise 
                                      (make-exn:fail:read:eof
                                       (format "Not enough chars on input (expected ~a, got ~a)" 
                                               n 
                                               (- n 0))
                                       (current-continuation-marks)
                                       ip))))))
      (copy-port cport op)))
  
  ; repeat-forever : (-> void) -> [diverges]
  ; repeatedly invokes the given thunk forever
  (define (repeat-forever thunk) (let loop () (thunk) (loop)))
  
  ; build-hash-table : listof (list X Y) -> equal-hash-table[X -> Y]
  ; builds a new hash-table mapping all given X's to their appropriate Y values
  (define (build-hash-table asl)
    (let ((ht (make-hash-table 'equal)))
      (for-each (lambda (item) (hash-table-put! ht (car item) (cadr item))) asl)
      ht))
  
  ; categorize : (X -> Y) (listof X) -> (listof (cons Y (listof X)))
  ; sorts the l into categories given by f
  (define (categorize f l)
    (let ((ht (make-hash-table 'equal)))
      (for-each 
       (lambda (i)
         (let ((key (f i)))
           (hash-table-put! ht key (cons i (hash-table-get ht key (lambda () '()))))))
       l)
      (hash-table-map ht cons)))
  
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
  
  (define (with-logging logfile f)
    (let* ((null-out (open-output-nowhere))
           (outport
            (if logfile
                (with-handlers ((exn:fail:filesystem? (lambda (e) null-out)))
                  (open-output-file logfile 'append))
                null-out)))
      (parameterize ([current-output-port outport])
        (f))))
  
  
  ;; pkg->info : PKG -> (symbol (-> TST) -> TST)
  ;; get an info.ss thunk for the given package
  (define (pkg->info p)
    (or
     (with-handlers ([exn:fail? (lambda (e) #f)])
       (get-info/full (pkg-path p)))
     (lambda (s thunk) (thunk))))
  
  ;; ============================================================
  ;; TREE STUFF
  ;; ============================================================
  
  ;; tree[X] ::= (make-branch X (listof tree[X])
  (define-struct branch (node children) (make-inspector))
  
  (define-struct (exn:fail:filesystem:no-directory exn:fail:filesystem) (dir))
  
  ;; directory->tree : directory (string -> bool) [nat | bool] [path->X] -> tree[X] | #f
  (define directory->tree
    (opt-lambda (directory valid-dir? [max-depth #f] [path->x path->string])
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
                 (map (lambda (d) (directory->tree d valid-dir? next-depth)) files))))))))
  
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
  (define tree-apply
    (opt-lambda (f t [depth 0])
      (let loop ((t t)
                 (priors '())
                 (curr-depth 0))
        (cond
          [(null? (branch-children t))
           (if (> curr-depth depth)
               (list (apply f (reverse (cons (branch-node t) priors))))
               '())]
          [else
           (let ((args (cons (branch-node t) priors)))
             (apply append
                    (map (lambda (x) (loop x args (add1 curr-depth))) (branch-children t))))]))))
  
  ;; tree->list : tree[x] -> sexp-tree[x]
  (define (tree->list tree)
    (cons (branch-node tree) (map tree->list (branch-children tree)))))
