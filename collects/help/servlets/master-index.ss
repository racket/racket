(module master-index mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "match.ss")
           (lib "dirs.ss" "setup")
           (lib "list.ss")
           (lib "match.ss")
           "../private/options.ss"
           "private/url.ss"
           "../private/standard-urls.ss"
           "private/html.ss")
  
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
  
  (define-struct entry (keyword host manual file label title) (make-inspector))
  (define entries (make-hash-table 'equal))
  
  ;;;
  ;;; HTML
  ;;;
  
  ; html-entry : entry -> xexpr
  ;   convert entry into link
  (define (html-entry the-entry)
    (match the-entry
      [($ entry keyword host manual file label title)
       `(div 'nbsp 'nbsp 'nbsp 'nbsp
             (a ([href ,(file-path->url host manual file label)])
                ,title))]))
  
  ; html-keyword : string -> xexpr
  ;   make xexpr with the keyword in bold followed by all associated entries
  (define (html-keyword keyword)
    `(div (b ,keyword)
          ,@(map html-entry (hash-table-get entries keyword))))
  
  ; html-master-index : -> xexpr
  (define (html-master-index)
    (let ([keywords (sort (hash-table-map entries (lambda (key val) key)) string<?)])
      `(div (h1 "Master Index")
            (p "This master index contains all keywords from the tex2page based manuals.")
            (p "All entries in keywords and hdindex files are thus included.")
            (p "Keywords from Scribble generated manuals are not included yet.")
            ,@(map html-keyword keywords))))
  
  ; file-path->url : string string path string -> string
  (define (file-path->url host manual file label)
    (string-append (url-static host manual file)
                   (if label (format "#~a" label) "")))
  
  ;;;
  ;;; ENTRIES
  ;;;
  
  ; add-entry! : entry ->
  ;   register the keyword of entry in the hash-table entries
  (define (add-entry! entry)
    (let* ([keyword (entry-keyword entry)]
           [old     (hash-table-get entries keyword (lambda () '()))])
      (hash-table-put! entries (entry-keyword entry) (cons entry old))))
  
  ; keyword->entry : string string list-from-keywords-file -> entry
  ;   convert list from keywords-file into an entry
  (define (keyword->entry host manual keyword-list)
    (match keyword-list
      [(keyword result-display html-file html-label title)
       (make-entry keyword host manual html-file html-label title)]
      [_
       (error 'keyword->entry
              "Expected a five element list: (<keyword> <result-to-display> <html-file> <html-label> <title>), got: "
              keyword-list)]))
  
  ; item->entry : string string list-from-hdindex-files -> entry
  ;   convert list from hdindex file into an entry
  (define (item->entry host manual item-list)
    (match item-list
      [(item html-file html-label title)
       (make-entry item host manual html-file html-label title)]
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
  ;   add all items in <dir>/keywords to the entries hash-table
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
  
  
  ; make the traversal
  (for-each add-keywords-in-sub-directories!
            host+dirs)
  )