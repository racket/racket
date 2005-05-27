#| planet-shared.ss -- shared client/server utility functions

Various common pieces of code that both the client and server need to access

==========================================================================================

|#

(module planet-shared mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "port.ss"))
  
  (provide (all-defined))
  
  (define-syntax (define-parameters stx)
    (syntax-case stx ()
      [(_ (name val) ...)
       (andmap identifier? (syntax-e #'(name ...)))
       #'(begin
           (provide name ...)
           (define name (make-parameter val)) ...)]))
  
  
  ; exn:i/o:protocol: exception indicating that a protocol error occured
  (define-struct (exn:i/o:protocol exn:fail:network) ())

  (define BUILD "build")
  
  ; ==========================================================================================
  ; CACHE LOGIC
  ; Handles checking the cache for an appropriate module
  ; ==========================================================================================

  ; language-version->repository : string -> string | #f
  ; finds the appropriate language version for the given repository
  (define (language-version->repository ver)
    (cond
      [(regexp-match #rx"20.+" ver) "207.1"]
      [(regexp-match #rx"3.+|29.|" ver) "300"]
      [else #f]))
  
  (define (version->description ver)
    (cond
      [(string=? ver "207.1") "20x"]
      [(string=? ver "300") "299.x-30x"]
      [else (error 'version->description "Expected one of 207.1 and 300, got ~a" ver)]))
  
  (define (legal-language? l)
    (and (language-version->repository l) #t))
  
  ; lookup-package : FULL-PKG-SPEC string[dirname] -> PKG | #f
  ; returns the directory pointing to the appropriate package in the cache, or #f if the given package
  ; isn't in the cache
  (define (lookup-package pkg cache-dir)
    (let ((pkg-dir (build-path (apply build-path cache-dir (pkg-spec-path pkg)) (pkg-spec-name pkg))))
      (if (directory-exists? pkg-dir)
          (get-best-match pkg pkg-dir)
          #f)))
  
  ; get-best-match :FULL-PKG-SPEC (listof string[directory-name]) -> PKG | #f
  ; gets the best version in the given subdirectory in the specified low and high version range
  ; or #f if there is no appropriate version
  (define (get-best-match pkg-spec path)
    (let ((major-version (if (pkg-spec-maj pkg-spec)
                             (let ((specified-number (number->string (pkg-spec-maj pkg-spec))))
                               (if (directory-exists? (build-path path specified-number))
                                   specified-number
                                   #f))
                             (get-highest-numbered-subdir path #f #f))))
      (if major-version
          (let ((minor-version (get-highest-numbered-subdir 
                                (build-path path major-version)
                                (pkg-spec-minor-lo pkg-spec)
                                (pkg-spec-minor-hi pkg-spec))))
            (if minor-version
                (make-pkg
                 (pkg-spec-name pkg-spec)
                 (pkg-spec-path pkg-spec)
                 (string->number major-version) 
                 (string->number minor-version)
                 (build-path path major-version minor-version))
                #f))
          #f)))
  
  ; get-highest-numbered-subdir : string (Nat | #f) (Nat | #f) -> string[subdir] | #f
  ; given a path, returns the subdirectory of that path with the highest numeric name or #f if
  ; none exists. Does not return the full path.
  (define (get-highest-numbered-subdir path lo hi)
    (define (valid-dir? d) 
      (and 
       (directory-exists? (build-path path d)) 
       (let ((n (string->number (path->string d))))
         (and n
              (or (not lo) (>= n lo))
              (or (not hi) (<= n hi))))))
    
    (unless (directory-exists? path) 
      (raise (make-exn:fail:filesystem 
              "Internal PLaneT error: inconsistent cache, directory does not exist" 
              (current-continuation-marks))))
    (max-string (map path->string (filter valid-dir? (directory-list path)))))
  
  ; FULL-PKG-SPEC : (make-pkg-spec string (Nat | #f) (Nat | #f) (Nat | #f) (listof string) (syntax | #f))
  (define-struct pkg-spec (name maj minor-lo minor-hi path stx) (make-inspector))
  ; PKG : string Nat Nat path
  (define-struct pkg (name route maj min path))
  
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
  
  ; max-string : listof string[digits] -> string | #f
  ; this odd little guy takes a list of strings that represent a number and returns the string
  ; that represents the maximum number among them, or #f if there were no numbers at all 
  (define (max-string strs)
    (if (null? strs)
        #f
        (let loop ((biggest (car strs))
                   (big-n (string->number (car strs)))
                   (rest (cdr strs)))
          (cond
            [(null? rest) biggest]
            [else
             (let* ([candidate (car rest)]
                    [test-n    (string->number candidate)])
               (if (> test-n big-n)
                   (loop candidate test-n (cdr rest))
                   (loop biggest big-n (cdr rest))))]))))
  
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