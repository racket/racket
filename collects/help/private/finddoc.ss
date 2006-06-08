(module finddoc mzscheme
  (require "path.ss"
	   "get-help-url.ss"
           (lib "dirs.ss" "setup"))

  (provide finddoc
	   finddoc-page
	   finddoc-page-anchor
	   find-doc-directory)
  
  ;; Creates a "file:" link into the indicated manual.
  ;; The link doesn't go to a particular anchor,
  ;; because "file:" does not support that.
  (define (finddoc manual index-key label)
    (let ([m (lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"file:~a\">~a</A>"
                  (build-path (car m) (caddr m))
                    label))))

  (define (finddoc-page-help manual index-key anchor?)
    (let ([m (lookup manual index-key "dummy")])
      (if (string? m)
          (error (format "Error finding index \"~a\" in manual \"~a\""
                         index-key manual))
          (if (servlet-path? (string->path (caddr m)))
              (if anchor?
                  (string-append (caddr m) "#" (cadddr m))
                  (caddr m))
              (get-help-url (build-path (list-ref m 0)
                                        (list-ref m 2))
                            (list-ref m 3))))))
  
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
  
  ;; returns either a string (failure) or
  ;; (list docdir index-key filename anchor title)
  (define (lookup manual index-key label)
    (let ([key (string->symbol manual)]
	  [docdir (find-doc-directory manual)])
      (let ([l (hash-table-get
		ht
		key
		(lambda ()
		  (let ([f (build-path docdir "hdindex")])
                    (if (file-exists? f)
                        (let ([l (with-input-from-file f read)])
                          (hash-table-put! ht key l)
                          l)
                        (error 'finddoc "manual index ~s not installed" manual)))))])
	(let ([m (assoc index-key l)])
	  (if m 
	      (cons docdir m)
	      (error 'finddoc "index key ~s not found in manual ~s" index-key manual))))))
  
  ;; finds the full path of the doc directory, if one exists
  ;; input is just the short name of the directory (as a path)
  (define (find-doc-directory doc)
    (ormap (lambda (d)
	     (let ([p (build-path d doc)])
	       (and (directory-exists? p)
		    p)))
	   (get-doc-search-dirs))))
