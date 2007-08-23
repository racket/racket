
(module getinfo mzscheme

  (require (lib "match.ss")
           (lib "list.ss")
           (lib "etc.ss")
           
           (lib "contract.ss")
           
           (lib "cachepath.ss" "planet"))

  (define info? (opt-> (symbol?) ((-> any/c)) any/c))
  (define path-or-string? (lambda (x) (or (path? x) (string? x))))
  
  ;; in addition to infodomain/compiled/cache.ss, getinfo will look in this 
  ;; file to find mappings. PLaneT uses this to put info about installed
  ;; planet packages.
  (define user-infotable (get-planet-cache-path))

  ;; get-info : (listof path-or-string) -> info/#f
  (define (get-info coll-path)
    (let* ([coll-path (map (lambda (x) (if (path? x) (path->string x) x)) coll-path)]
	   [dir (apply collection-path coll-path)])
      (get-info/full dir)))

  ;; get-info/full : path -> info/#f
  (define (get-info/full dir)
    (let ([file (build-path dir "info.ss")])
      (if (file-exists? file)
	  (begin
	    (with-input-from-file file
	      (lambda ()
		(let ([r (read)])
		  (unless (eof-object? (read))
		    (error "info.ss file has multiple expressions in ~a" dir))
		  (match r
		    [('module 'info '(lib "infotab.ss" "setup")
		       expr ...)
		     'ok]
		    [else (error 
			   'get-info
			   "info file does not contain a module of the right shape: \"~a\""
			   file)]))))
	    (dynamic-require file '#%info-lookup))
	  #f)))

  ;; directory-record = (make-directory-record nat nat key path (listof symbol))
  ;; eg: (make-directory-record 1 0 '(lib "mzlib") #"mzlib" '(name))
  (define-struct directory-record (maj min spec path syms))
  
  (define-struct table (insert   ; directory-record (listof directory-record)
                                 ;  -> (listof directory-record)
                        ht       ; hashtable[symbol -o> directory-record]
                        paths    ; (listof (cons path boolean))
                        ))
  
  (define preferred-table #f)
  (define all-available-table #f)

  ;; reset-relevant-directories-state! : -> void
  (define (reset-relevant-directories-state!)
    (set! preferred-table 
          (make-table 
           (lambda (i l)
             (cond
               [(null? l)
                (list i)]
               [else 
                (match-let ([($ directory-record my-maj my-min _ _ _) i]
                            [($ directory-record their-maj their-min _ _ _) (car l)])
                  (if (or (> my-maj their-maj)
                          (and (= my-maj their-maj) (>= my-min their-min)))
                      (list i)
                      l))]))
           #f #f))
    (set! all-available-table
          (make-table cons #f #f)))
  
  (reset-relevant-directories-state!)

  ;; populate-table : table -> void
  (define (populate-table! t)
    ;; Use the colls ht because a collection might be in multiple
    ;; collection paths, and we only want one
    (let ([colls (make-hash-table 'equal)])
      (for-each
       (lambda (f+root-dir)
         (let ([f (car f+root-dir)]
               [root-dir (cdr f+root-dir)])
           (when (file-exists? f)
             (for-each 
              (lambda (i)
                (match i
                  [((? bytes? pathbytes) 
                    ((? symbol? fields) ...)
                    key ;; anything is okay here
                    (? integer? maj)
                    (? integer? min))
                   (let ((old-items (hash-table-get 
                                     colls
                                     key
                                     (lambda () '())))
                         (new-item
                          (make-directory-record
                           maj
                           min
                           key
                           (let ([p (bytes->path pathbytes)])
                             (if (and (relative-path? p) root-dir)
                                 (build-path root-dir p)
                                 p))
                           fields)))
                     (hash-table-put! colls 
                                      key
                                      ((table-insert t) new-item old-items)))]
                  [_
                   (error 'find-relevant-directories
                          "bad info-domain cache entry: ~e in: ~a" 
                          i
                          f)]))
              (let ([l (with-input-from-file f read)])
                (cond
                 [(list? l) l]
                 [(eof-object? l) '()] ;; allow completely empty files
                 [else
                  (error 'find-relevant-directories
                         "bad info-domain cache file: ~a" 
                         f)]))))))
       (reverse (table-paths t)))
      ;; For each coll, invert the mapping, adding the col name to the list
      ;; for each sym: 
      (hash-table-for-each
       colls
       (lambda (key vals)
         (for-each
          (lambda (val)
            (match val
              [($ directory-record maj min spec path syms)
               (for-each
                (lambda (sym)
                  (hash-table-put! 
                   (table-ht t)
                   sym
                   (cons val
                         (hash-table-get (table-ht t) sym (lambda () null)))))
                syms)]
              [_ (error 'get-info 
                        "Internal error: invalid info-domain value format: ~s" val)]))
          vals)))))
  
  (define find-relevant-directories
    (opt-lambda (syms [key 'preferred])
      (map directory-record-path (find-relevant-directory-records syms key))))
  
  (define find-relevant-directory-records
    (opt-lambda (syms [key 'preferred])
      (define t (cond 
                  [(eq? key 'preferred) preferred-table]
                  [(eq? key 'all-available) all-available-table]
                  [else (error 'find-relevant-directories "Invalid key: ~s" key)]))
      
      ;; A list of (cons cache.ss-path root-dir-path)
      ;;  If root-dir-path is not #f, then paths in the cache.ss
      ;;  file are relative to it. #f is used for the planet cache.ss file.
      (define search-path 
        (cons (cons user-infotable #f)
              (map (lambda (coll)
                     (cons (build-path coll "info-domain" "compiled" "cache.ss")
                           coll))
                   (current-library-collection-paths))))
      
      (unless (equal? (table-paths t) search-path)
        (set-table-ht! t (make-hash-table))
        (set-table-paths! t search-path)
        (populate-table! t))
      
      (let ([unsorted
             (if (= (length syms) 1)
                 ;; Simple case: look up in table
                 (hash-table-get (table-ht t) (car syms) (lambda () null))
                 ;; Use a hash table, because the same collection might work
                 ;; for multiple syms
                 (let ([result (make-hash-table 'equal)])
                   (for-each
                    (lambda (sym)
                      (let ([l (hash-table-get (table-ht t) sym (lambda () null))])
                        (for-each (lambda (c) (hash-table-put! result c #t))
                                  l)))
                    syms)
                   ;; Extract the relevant collections:
                   (hash-table-map result (lambda (k v) k))))])
        (sort unsorted
              (lambda (a b)
                (compare-directories (directory-record-path a)
                                     (directory-record-path b)))))))
  
  (define (compare-directories a b)
    (bytes<? (dir->sort-key a) (dir->sort-key b)))
  
  ;; dir->sort-key : path -> bytes
  ;; extracts the name of the directory, dropping any "."s it finds at the ends.
  (define (dir->sort-key path)
    (let-values ([(base name dir?) (split-path path)])
      (cond
        [(eq? name 'same) (dir->sort-key base)]
        [else (path->bytes name)])))
  
  (provide/contract
   (reset-relevant-directories-state! (-> any))
   (get-info ((listof path-or-string?) . -> . (or/c info? boolean?)))
   (get-info/full (path? . -> . (or/c info? boolean?)))
   (find-relevant-directories (opt-> ((listof symbol?))
                                     ((lambda (x) (or (eq? x 'preferred)
                                                      (eq? x 'all-available))))
                                     (listof path?)))
   (struct directory-record
           ([maj integer?]
            [min integer?]
            [spec any/c]
            [path path?]
            [syms (listof symbol?)]))
   (find-relevant-directory-records (opt-> ((listof symbol?))
                                           ((lambda (x) (or (eq? x 'preferred)
                                                            (eq? x 'all-available))))
                                           (listof directory-record?)))))
