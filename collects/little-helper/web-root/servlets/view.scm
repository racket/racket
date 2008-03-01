(module view mzscheme
  (provide (all-defined))
  
  (require mzlib/match 
           net/url
           setup/dirs
           web-server/servlet
           (all-except xml/xml 
                       document document? make-document struct:document)
           "private/html.scm"
           "private/mime.scm"
           "private/request.scm"
           "../../indexer/planet/intersperse.scm"
           "../../indexer/planet/file.scm"
           (prefix home: "home.scm"))
  
  (define interface-version 'v1)
  (define timeout 6000)
  (define (start request)
    (current-request request)
    (with-errors-to-browser send/finish 
      (λ () (do-view-page request))))
  
  ;;; VIEW PAGE
  
  (define (do-view-page request)
    (begin (write (map path/param-path (url-path (request-uri request))) 
                    (current-error-port))
             (newline (current-error-port)))
    ; extract repository-id and document-id from the url and dispatch
    ; index.html and scribble.css is handled specially
    (match (map path/param-path (url-path (request-uri request)))
      [("servlets" "view.scm" "scribble.css")
       (file-verbatim "scribble.css")]
      [("servlets" "view.scm" "index.html")
       (home:do-home-page)]
      [("servlets" "view.scm")     ; .../view.scm
       ; redirect to  .../views.scm/
       (make-response/full 
        303 "See Other" (current-seconds) TEXT/HTML-MIME-TYPE
        (list (make-header #"Location" (string->bytes/utf-8 "view.scm/"))
              (make-header #"Cache-Control" #"no-cache")
              (make-header #"Expires" #"Thu, 01 Jan 1970 00:00:00 GMT"))
        (list doctype-HTML-4.01-Transitional ""))]
      [("servlets" "view.scm" "")  ; .../view.scm/
       (home:do-home-page)]
      [("servlets" "view.scm" repid . docid-pieces)
       (let ([docid (apply string-append
                           (intersperse "/" docid-pieces))]
             [file (apply build-path `(,(find-doc-dir) ,repid ,@docid-pieces))])
         (cond [(not (file-exists? file))
                (html-document-not-found repid docid)]
               [(text-mime-type? file)
                (cond [(not (member repid '("help" "release-notes")))
                       (html-view-file file)]
                      [else  ; txt-files
                       (html-view-txt-file file)])]
               [else
                ; binary files such as pngs, gifs, etc.
                (file-verbatim file)]))]
      [_ (error "huh - was the file view.scm renamed?")]))
  
  (define (html-view-file file)
    (list #"text/html"
          (string-append (xexpr->string
                          (html-page
                           #:title (title (format "View: ~a" file))
                           #:body  ""))
                         (file->string file))))
  
  (define (html-view-txt-file file)
    (html-page
     #:title (title (format "View: ~a" file))
     #:body  `(pre ,(file->string file))))
  
  
  (define (file-verbatim path)
    (list (get-mime-type path)
          (file->bytes path)))
  
  (define (html-error-page title-text text)
    (html-page
     #:title (title title-text)
     #:body `(div (h1 ,title-text)
                  ,text)))
  
  (define (html-file-not-found-page file)
    (html-error-page "File Not Found"
                     (format "The file '~a' was not found." file)))
  
  (define (html-document-not-found rep-name docid)
    (html-error-page 
     "Document Not Found"
     (format "The repository '~a' exists, but it does not contain the document: ~a"
             rep-name docid)))
  
  (define (html-repository-not-found-page repid docid)
    (html-error-page
     "Repository Not Found"
     (format "No repository has the name '~a' ~a" repid
             " please file a bug report.")))
  
  )