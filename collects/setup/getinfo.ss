
(module getinfo mzscheme

  (require (lib "match.ss")
           (lib "list.ss")
           (lib "etc.ss")
           
           (lib "contract.ss")
           
           (lib "planet-archives.ss" "planet"))

  (define info? (opt-> (symbol?) ((-> any/c)) any/c))
  (define path-or-string? (lambda (x) (or (path? x) (string? x))))
  
  (provide/contract
   (get-info ((listof path-or-string?) . -> . (union info? boolean?)))
   (get-info/full (path? . -> . (union info? boolean?)))
   (find-relevant-directories (opt-> ((listof symbol?))
                                     ((lambda (x) (or (eq? x 'preferred)
                                                      (eq? x 'all-available))))
                                     (listof path?))))


  
  ;; in addition to infodomain/compiled/cache.ss, getinfo will look in this 
  ;; file to find mappings. PLaneT uses this to put info about installed
  ;; planet packages.
  (define user-infotable (get-planet-cache-path))
  
  (define (get-info coll-path)
    (let* ([coll-path (map (lambda (x) (if (path? x) (path->string x) x)) coll-path)]
	   [dir (apply collection-path coll-path)])
      (get-info/full dir)))
  
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

  ;; item : (list path (listof symbol) nat nat)
  
  (define-struct table (insert   ; item * listof item -> listof item
                        ht       ; hashtable[key -o> item]
                        paths    ; listof path
                        ))
  
  (define preferred-table 
    (make-table 
     (lambda (i l)
       (cond
         [(null? l)
          (list i)]
         [else 
          (match-let ([(_ _ my-maj my-min) i]
                      [(_ _ their-maj their-min) (car l)])
            (if
             (or (> my-maj their-maj)
                 (and (= my-maj their-maj) (>= my-min their-min)))
               (list i)
               l))]))
     #f #f))
  
  (define all-available-table (make-table cons #f #f))
  
  (define (populate-table! t)
    ;; Use the colls ht because a collection might be in multiple
    ;;  collection paths, and we only want one
    (let ([colls (make-hash-table 'equal)])
      (for-each (lambda (f)
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
                                (new-item (list (bytes->path pathbytes) fields maj min)))
                            (hash-table-put! colls 
                                             key
                                             ((table-insert t) new-item old-items)))]
                         [_
                          (error 'find-relevant-directories
                                 "bad info-domain cache entry: ~e in: ~a" 
                                 i
                                 f)]))
                     (let ([l (with-input-from-file f read)])
                       (unless (list? l)
                         (error 'find-relevant-directories
                                "bad info-domain cache file: ~a" 
                                f))
                       l))))
		(reverse (table-paths t)))
      ;; For each coll, invert the mapping, adding the col name to the list for each sym: 
      (hash-table-for-each colls
			   (lambda (key vals)
                             (for-each
                              (lambda (val)
                                (match val
                                  [(path syms maj min)
                                   (for-each (lambda (sym)
                                               (hash-table-put! 
                                                (table-ht t)
                                                sym
                                                (cons path (hash-table-get (table-ht t) sym (lambda () null)))))
                                             syms)]
                                  [_ (error 'get-info 
                                            "Internal error: invalid info-domain value format: ~s" val)]))
                              vals)))))

  (define find-relevant-directories
    (opt-lambda (syms [key 'preferred])
      (define t (cond 
                  [(eq? key 'preferred) preferred-table]
                  [(eq? key 'all-available) all-available-table]
                  [else (error 'find-relevant-directories "Invalid key: ~s" key)]))
 
      (define search-path 
        (cons user-infotable 
              (map (lambda (coll) (build-path coll "info-domain" "compiled" "cache.ss"))
                   (current-library-collection-paths))))
      
      (unless (equal? (table-paths t) search-path)
        (set-table-ht! t (make-hash-table))
        (set-table-paths! t search-path)
        (populate-table! t))
      
      (let ([unsorted (if (= (length syms) 1)
                          ;; Simple case: look up in table
                          (hash-table-get (table-ht t) (car syms) (lambda () null))
                          ;; Use a hash table, because the same collection might work for multiple syms
                          (let ([result (make-hash-table 'equal)])
                            (for-each (lambda (sym)
                                        (let ([l (hash-table-get (table-ht t) sym (lambda () null))])
                                          (for-each (lambda (c) (hash-table-put! result c #t))
                                                    l)))
                                      syms)
                            ;; Extract the relevant collections:
                            (hash-table-map result (lambda (k v) k))))])
        (quicksort unsorted compare-directories))))
  
  (define (compare-directories a b)
    (let-values ([(base1 name1 dir?1) (split-path a)]
                 [(base2 name2 dir?2) (split-path b)])
      (bytes<? (path->bytes name1) (path->bytes name2)))))
