(module finddoc mzscheme
  (require "path.ss")

  (provide finddoc
	   findreldoc
           finddoc-page
	   finddoc-page-anchor)
  
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

  ;; Given a Unix-style relative path to reach the "doc"
  ;; collection, creates a link that can go to a
  ;; particular anchor.
  (define (findreldoc todocs manual index-key label)
    (let ([m (lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"~a/~a/~a#~a\">~a</A>"
                  todocs
                  manual
                  (caddr m)
                  (cadddr m)
                  label))))

  (define (finddoc-page-help manual index-key anchor?)
    (let ([m (lookup manual index-key "dummy")])
      (if (string? m)
          (error (format "Error finding index \"~a\" in manual \"~a\""
                         index-key manual))
          (let ([path (if anchor?
                          (string-append (caddr m) "#" (cadddr m))
                          (caddr m))])
            (if (servlet-path? path)
                path
                (format "/doc/~a/~a" manual path))))))
  
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
	  [docdir (build-path (collection-path "doc") manual)])
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
	      (error 'finddoc "index key ~s not found in manual ~s" index-key manual)))))))


