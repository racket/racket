(module master-index mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "match.ss")
           (lib "dirs.ss" "setup")
           (lib "list.ss")
           (lib "match.ss")
           (lib "uri-codec.ss" "net")
           "../private/options.ss"
           "private/url.ss"
           "../private/standard-urls.ss"
           "private/html.ss"
           "../private/search.ss")
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start request)
    (with-errors-to-browser 
        send/finish
      (lambda ()
        (html-page 
         #:title "Master Index"
         #:top (case (helpdesk-platform)
                 [(internal-browser) '()]
                 [(internal-browser-simple) '()]
                 [else (html-top request)])
         #:body (html-master-index)))))

  ;;;
  ;;; ENTRIES
  ;;;
  
  (define-struct entry (keyword) (make-inspector))
  (define-struct (manual-entry entry) (host manual file label title) (make-inspector))
  (define-struct (doc.txt-entry entry) (file offset title))
  
  (define entries (make-hash-table 'equal))
  
  ;;;
  ;;; HTML
  ;;;
  
  ; html-entry : entry -> xexpr
  ;   convert entry into link
  (define (html-entry the-entry)
    (match the-entry
      [($ manual-entry keyword host manual file label title)
       `(div 'nbsp 'nbsp 'nbsp 'nbsp
             (a ([href ,(manual-file-path->url host manual file label)])
                ,title))]
      [($ doc.txt-entry keyword file offset title)
       `(div 'nbsp 'nbsp 'nbsp 'nbsp
             (a ([href ,(doc.txt-file-path->url file title keyword offset)])
                ,title))]
      [_ (error)]))
  
  ; html-keyword : string -> xexpr
  ;   make xexpr with the keyword in bold followed by all associated entries
  (define (html-keyword keyword)
    `(div (b ,keyword)
          ,@(map html-entry (hash-table-get entries keyword))))
  
  ; html-master-index : -> xexpr
  (define (html-master-index)
    (let ([keywords (sort (hash-table-map entries (lambda (key val) key)) 
                          (lambda (s1 s2) (string<? (string-downcase s1) (string-downcase s2))))])
      `(div (h1 "Master Index")
            (p "This master index contains all keywords from the tex2page based manuals as well as doc.txt files.")
            (p "All entries in keywords and hdindex files are thus included.")
            (p "Keywords from Scribble generated manuals are not included yet.")
            (p "PLaneT documentation is not included.")
            ,@(map html-keyword keywords))))
  
  ; manual-file-path->url : string string path string -> string
  (define (manual-file-path->url host manual file label)
    (string-append (url-static host manual file)
                   (if label (format "#~a" label) "")))
  
  (define (doc.txt-file-path->url file caption name offset)
    (format "/servlets/doc-anchor.ss?file=~a&caption=~a&name=~a&offset=~a#temp"
            (path->string file)
            (uri-encode caption)
            (uri-encode name)
            offset))
    
  
  ;;;
  ;;; ENTRIES
  ;;;
  
  ; add-entry! : entry ->
  ;   register the keyword of entry in the hash-table entries
  (define (add-entry! entry)
    (let* ([keyword (entry-keyword entry)]
           [old     (hash-table-get entries keyword (lambda () '()))])
      (hash-table-put! entries (entry-keyword entry) (cons entry old))))
  
  ; keyword->entry : string string list-from-keywords-file -> manual-entry
  ;   convert list from keywords-file into an manual-entry
  (define (keyword->entry host manual keyword-list)
    (match keyword-list
      [(keyword result-display html-file html-label title)
       (make-manual-entry keyword host manual html-file html-label title)]
      [_
       (error 'keyword->entry
              "Expected a five element list: (<keyword> <result-to-display> <html-file> <html-label> <title>), got: "
              keyword-list)]))
  
  ; item->entry : string string list-from-hdindex-files -> manual-entry
  ;   convert list from hdindex file into an entry
  (define (item->entry host manual item-list)
    (match item-list
      [(item html-file html-label title)
       (make-manual-entry item host manual html-file html-label title)]
      [_
       (error 'item->entry
              "Expected a four element list: (<item> <html-file> <html-label> <title>), got: "
              item-list)]))
  
  ;;;
  ;;; TRAVERSAL
  ;;;
  
  ; add-keywords-in-directory! : string string path ->
  ;   add all keywords in <dir>/keywords to the entries hash-table
  (define (add-keywords-in-directory! host manual dir)
    (when (directory-exists? dir)
      (let ([keywords-path (build-path dir "keywords")]) 
        (when (file-exists? keywords-path)
          (with-input-from-file keywords-path
            (lambda ()
              (let ([keyword-entries (read)])
                (for-each (lambda (k) (add-entry! (keyword->entry host manual k)))
                          keyword-entries))))))))
  
  ; add-items-in-directory! : string string path ->
  ;   add all items in <dir>/hdindex to the entries hash-table
  (define (add-items-in-directory! host manual dir)
    (when (directory-exists? dir)
      (let ([items-path (build-path dir "hdindex")]) 
        (when (file-exists? items-path)
          (with-input-from-file items-path
            (lambda ()
              (let ([item-entries (read)])
                (for-each (lambda (k) (add-entry! (item->entry host manual k)))
                          item-entries))))))))
  
  ; add-keywords-and-items-in-sub-directories! : (cons string path) -> 
  ;   add all keywords in the keywords-files path/*/keywords to the hash-table entries
  ;   add all items in the hdindex-files path/*/hdindex to the hash-table entries
  (define (add-keywords-in-sub-directories! host+dir)
    (match host+dir
      [(host . dir)
       (when (directory-exists? dir)
         (for-each (lambda (manual)
                     (add-keywords-in-directory! host manual (build-path dir manual))
                     (add-items-in-directory! host manual (build-path dir manual)))
                   (directory-list dir)))]))

  (define (add-keywords-in-doc.txt-file doc name)
    (let ([ht (make-hash-table 'equal)])
      (load-txt-keywords-into-hash-table ht doc)
      (hash-table-for-each 
       ht (lambda (key val)
            (for-each (lambda (item)
                        (match item
                          [(keyword title doc.txt-path offset _)
                           (add-entry! 
                            (make-doc.txt-entry keyword key offset title))]))
                      val)))))
  
  ; add-keywords-in-doc.txt-files : ->
  (define (add-keywords-in-doc.txt-files)
    (reset-doc-lists)
    (let-values ([(pathss names types) (extract-doc-txt)])
      (for-each 
       (lambda (paths name type)
         (match paths
           [(base-path doc-txt)
            (add-keywords-in-doc.txt-file paths name)]))
       pathss names types)))
  
  
  ; make the traversal
  (for-each add-keywords-in-sub-directories!
            host+dirs)
  (add-keywords-in-doc.txt-files)
  )

