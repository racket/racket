(module colldocs mzscheme
  (require (lib "list.ss")
           (lib "getinfo.ss" "setup")
           (lib "contract.ss"))
  
  (provide/contract 
   [colldocs (-> (values (listof (list/c path? path?))
                         (listof string?)))])

  (define (colldocs)
    (let loop ([dirs (find-relevant-directories '(doc.txt) 'all-available)]
               [docs null]
               [names null])
      (cond
        [(null? dirs)
         (values docs names)]
        [else (let* ([dir (car dirs)]
                     [info-proc (get-info/full dir)])
                (if info-proc
                    (let ([doc.txt-path (info-proc 'doc.txt (lambda () #f))]
                          [name (info-proc 'name (lambda () #f))])
                      (if (and (path-string? doc.txt-path)
                               (string? name))
                          (loop (cdr dirs)
                                (cons (list dir
                                            (string->path doc.txt-path))
                                      docs)
                                (cons name names))
                          (loop (cdr dirs) docs names)))
                    (loop (cdr dirs) docs names)))])))
                        
  
  ; Gets a list of collections that contain a doc.txt file
  ; returns two parallel lists.
  ; the first has the locations of the docs and the second is their names.
  #;
  (define (colldocs)
    (let loop ([collection-paths (current-library-collection-paths)]
               [docs null]
               [names null])
      (cond
	[(null? collection-paths)
	 (let* ([collections-docs (map cons docs names)]
		[l (quicksort collections-docs
			      (lambda (a b) (string<? (cdr a) (cdr b))))])
	   (values (map car l) (map cdr l)))]
	[else (let ([path (car collection-paths)])
		(let cloop ([l (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
                                 (directory-list path))]
			    [path path]
			    [collpath null]
			    [docs docs]
			    [names names])
                  (cond
                    [(null? l) (if (null? collpath)
                                   (loop (cdr collection-paths) docs names)
                                   (values docs names))]
                    [else 
                     (let* ([coll (car l)]
                            [colldir (build-path path coll)])
                       (cond
                         [(and (directory-exists? colldir)
                               (not (member (path->string coll) names)))
                          (let* ([lcollpath (append collpath (list coll))]
                                 [doc-txt-file (list colldir (string->path "doc.txt"))]
                                 [this? (file-exists? (apply build-path doc-txt-file))])
                            (let-values ([(sub-docs sub-names)
                                          (with-handlers ([exn:fail:filesystem?
                                                           (lambda (x) (values null null))])
                                            (let ([info-proc/f (get-info lcollpath)])
                                              (if info-proc/f
                                                  (let ([l (info-proc/f 'doc-sub-collections (lambda () null))])
                                                    (cloop (map string->path l) colldir lcollpath null null))
                                                  (values null null))))])
                              (let ([sub-names (map (lambda (s) (string-append (path->string coll) " " s)) sub-names)])
                                (let-values ([(ldocs lnames)
                                              (if this?
                                                  (values (cons doc-txt-file sub-docs)
                                                          (cons (path->string coll) sub-names))
                                                  (values sub-docs sub-names))])
                                  (cloop (cdr l) path collpath (append ldocs docs) (append lnames names))))))]
                         [else (cloop (cdr l) path collpath docs names)]))])))]))))
