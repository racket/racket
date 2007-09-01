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
                 [else               (html-top request)])
         #:body (html-master-index)))))
  
  
  (define-struct entry (keyword host manual file label title) (make-inspector))
  (define entries (make-hash-table 'equal))
  
  ;;;
  ;;; HTML
  ;;;
  
  (define (html-entry the-entry)
    (match the-entry
      [($ entry keyword host manual file label title)
       `(div 'nbsp 'nbsp 'nbsp 'nbsp
             (a ([href ,(file-path->url host manual file label)])
                ,title))]))
  
  (define (html-keyword keyword)
    `(div (b ,keyword)
          ,@(map html-entry (hash-table-get entries keyword))))
  
  (define (html-master-index)
    (let ([keywords (sort (hash-table-map entries (lambda (key val) key)) string<?)])
      `(div (h1 "Master Index")
            (p "This master index contains (for now) all keywords from the tex2page based manuals.")
            ,@(map html-keyword keywords))))
  
  (define (file-path->url host manual file label)
    (string-append (url-static host manual file)
                   (if label (format "#~a" label) "")))
  
  ;;;
  ;;; ENTRIES
  ;;;
  
  (define (add-entry! entry)
    (let* ([keyword (entry-keyword entry)]
           [old     (hash-table-get entries keyword (lambda () '()))])
      (hash-table-put! entries (entry-keyword entry) (cons entry old))))
  
  (define (keyword->entry host manual keyword-list)
    (match keyword-list
      [(keyword result-display html-file html-label title)
       (make-entry keyword host manual html-file html-label title)]
      [_
       (error 'keyword->list 
              "Expected a four element list: (<keyword> <result-to-display> <html-file> <html-label> <title>), got: "
              keyword-list)]))
  
  ;;;
  ;;; TRAVERSAL
  ;;;
  
  (define (add-keywords-in-directory! host manual dir)
    (when (directory-exists? dir)
      (let ([keywords-path (build-path dir "keywords")]) 
        (when (file-exists? keywords-path)
          (with-input-from-file keywords-path
            (lambda ()
              (let ([keyword-entries (read)])
                (for-each (lambda (k) (add-entry! (keyword->entry host manual k)))
                          keyword-entries))))))))
  
  (define (add-keywords-in-sub-directories! host+dir)
    (match host+dir
      [(host . dir)
       (when (directory-exists? dir)
         (for-each (lambda (manual)
                     (add-keywords-in-directory! host manual
                                                 (build-path dir manual)))
                   (directory-list dir)))]))
  
  (for-each add-keywords-in-sub-directories!
            host+dirs)
  )