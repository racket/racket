(module installed-components mzscheme
  (require (lib "list.ss")
           (lib "xml.ss" "xml")
           (lib "getinfo.ss" "setup")
	   (lib "uri-codec.ss" "net")
           (lib "util.ss" "help" "servlets" "private"))

  (provide help-desk:installed-components)
  
  ;; comp = (make-comp string xexpr)
  ;; this represents a collection with a blurb field.
  ;; the name names the collection and the xml is its xexpr blurb
  (define-struct comp (name xml))
  
  ;; help-desk:installed-components url : -> (listof (list string xexpr))
  ;; represents all of the collections with blurb fields 
  (define (help-desk:installed-components)
    (let ([comps
           (sort (filter (lambda (x) x)
                         (map (lambda (c) (get-blurb c)) (all-collections)))
                 comp<=?)])
      (map comp-xml comps)))
  
  ;; all-collections : ->  (lisof string)
  ;; returns a list of the collections from the current-library-collections-path parameter
  (define (all-collections)
    (let ([colls (make-hash-table 'equal)])
      (for-each
       (lambda (collection-path-dir)
         (when (directory-exists? collection-path-dir)
           (for-each
            (lambda (collection)
              (when (and (directory-exists? (build-path collection-path-dir collection))
                         (not (member (path->bytes collection) '(#"CVS" #".svn"))))
                (hash-table-put! colls collection #t)))
            (directory-list collection-path-dir))))
       (current-library-collection-paths))
      (sort (hash-table-map colls (lambda (x v) x))
            (lambda (x y) (string<=? (path->string x) (path->string y))))))
  
  ;; get-blurb : string url -> xexpr
  ;; builds the xexpr for a collection, based on its name a blurb
  (define (get-blurb collection)
    (let/ec k
      (let ([proc (with-handlers ([(lambda (x) (not (exn:break? x)))
                                   (lambda (x) #f)])
                    (get-info (list collection)))])
        (unless proc
          (k #f))
        (let* ([name (with-handlers ([(lambda (x) #t)
                                      (lambda (x)
                                        (k
                                         (make-comp
                                          collection
                                          `(li 
                                            (font ((color "forestgreen")) (b () ,collection))
                                            (p
                                             (font
                                              ((color "red"))
                                              (i ,(format "error during 'name: ~a"
                                                          (if (exn? x)
                                                              (exn-message x)
                                                              x)))))))))])
                       (proc 'name (lambda () (k #f))))]
               [blurb (with-handlers ([(lambda (x) #t)
                                       (lambda (x)
                                         (k
                                          (make-comp
                                           collection
                                           `(li 
                                             (font ((color "forestgreen")) (b () ,name))
                                             (br)
                                             (font ((color "red"))
                                                   (i
                                                    ,(format "error during 'blurb: ~a"
                                                             (if (exn? x)
                                                                 (exn-message x)
                                                                 x))))))))])
                        (proc 'blurb (lambda () (k #f))))]
               [blurb-ok? (andmap xexpr? blurb)])
          (make-comp
           name
           `(li
             (font ((color "forest green"))
                   (b ,name))
             (br)
             ,@(append
                (if blurb-ok?
                    blurb
                    (list `(font ((color "red"))
                                 "blurb was not a list of xexprs")))
		(let ([fname (build-path (collection-path collection) "doc.txt")])
		  (if (file-exists? fname)
		      (list
		       " See "
		       `(A ((HREF ,(format 
				    "/servlets/doc-anchor.ss?file=~a&caption=Documentation for the ~a collection&name=~a" 
				    (uri-encode (path->string fname))
			            collection
			            collection)))
                         "the documentation")
                     " for more information.")
                    null)))))))))
  
  ;; build-string-from-comp : comp -> string
  ;; constructs a string version of the xexpr from a comp
  (define (build-string-from-comp comp)
    (let ([blurb (comp-xml comp)]
          [p (open-output-string)])
      (write-xml/content
       (xexpr->xml
        blurb)
       p)
      (newline p)
      (newline p)
      (get-output-string p)))
  
  ;; comp<=? : comp comp -> boolean
  ;; compares two comps for sorting
  (define (comp<=? ca cb) (string<=? (comp-name ca) (comp-name cb))))
