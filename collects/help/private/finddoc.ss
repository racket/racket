(module finddoc mzscheme
  (require (lib "dirs.ss" "setup")
           (lib "match.ss")
           "path.ss"
	   "get-help-url.ss")
  
  (provide finddoc
	   finddoc-page
	   finddoc-page-anchor
	   find-doc-directory)
  
  ;; Creates a "file:" link into the indicated manual.
  ;; The link doesn't go to a particular anchor,
  ;; because "file:" does not support that.
  (define (finddoc manual index-key label)
    (match (lookup manual index-key label) 
      [(docdir index-key filename anchor title)
       `(a ((href ,(string-append 
                    "file:" (path->string (build-path docdir filename)))))
           ,label)]
      [m m]))
  
  ; finddoc-page-help : string string boolean -> string
  ;   return url to the page where index-key is in manual,
  ;   optionally append an anchor
  (define (finddoc-page-help manual index-key anchor?)
    (match (lookup manual index-key "dummy")
      [(docdir index-key filename anchor title)
       (cond
         [(servlet-path? (string->path filename))
          (string-append 
           filename (if anchor? (string-append "#" anchor) ""))]
         [else
          (get-help-url (build-path docdir filename) anchor)])]
      [_ (error (format "Error finding index \"~a\" in manual \"~a\""
                        index-key manual))]))
  
  ; finddoc-page : string string -> string
  ; returns path for use by PLT Web server
  ;  path is of form /doc/manual/page, or
  ;  /servlet/<rest-of-path>
  (define (finddoc-page manual index-key)
    (finddoc-page-help manual index-key #f))
  
  ; finddoc-page-anchor : string string -> string
  ; returns path (with anchor) for use by PLT Web server
  ;  path is of form /doc/manual/page#anchor, or
  ;  /servlet/<rest-of-path>#anchor
  (define (finddoc-page-anchor manual index-key)
    (finddoc-page-help manual index-key #t))
  
  (define ht (make-hash-table))
  
  ;; returns (list docdir index-key filename anchor title)
  ;; or throws an error
  (define (lookup manual index-key label)
    (let* ([key (string->symbol manual)]
           [docdir (find-doc-directory manual)]
           [l (hash-table-get ht key
                              (lambda ()
                                (let ([f (and docdir (build-path docdir "hdindex"))])
                                  (if (and f (file-exists? f))
                                      (let ([l (with-input-from-file f read)])
                                        (hash-table-put! ht key l)
                                        l)
                                      (error 'finddoc "manual index ~s not installed" manual)))))]
           [m (assoc index-key l)])
      (if m
          (cons docdir m)
          (error 'finddoc "index key ~s not found in manual ~s" index-key manual))))
  
  ;; finds the full path of the doc directory, if one exists
  ;; input is just the short name of the directory (as a path)
  (define (find-doc-directory doc)
    (ormap (lambda (d)
	     (let ([p (build-path d doc)])
	       (and (directory-exists? p)
		    p)))
	   (get-doc-search-dirs))))
