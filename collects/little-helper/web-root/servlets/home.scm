(module home mzscheme
  (provide interface-version timeout start)
  (provide do-home-page)
  
  (require scheme/port
           setup/dirs
           (all-except (lib "xml.ss" "xml") 
                       document document? make-document struct:document)
           (lib "servlet.ss" "web-server")
           "private/request.scm"
           "private/html.scm")
  
  (define interface-version 'v1)
  (define timeout 6000)
  (define (start request)
    (current-request request)
    (with-errors-to-browser send/finish 
      do-home-page))
  
  (define (do-home-page)
    (html-home-page))
  
  (define (file->string file)
    (let ([s (open-output-string)])
      (with-input-from-file file
        (λ ()
          (copy-port (current-input-port) s)))
      (get-output-string s)))
  
  
  (define (html-home-page)
    (make-response/full 200               ; code
                        "Okay"            ; message
                        (current-seconds) ; seconds
                        TEXT/HTML-MIME-TYPE
                        ;                 ; headers  
                        (list (make-header #"Pragma" #"No-cache")
                              (make-header #"Cache-Control" #"no-cache")
                              (make-header #"Expires" #"Thu, 01 Jan 1970 00:00:00 GMT"))
                        (list doctype-HTML-4.01-Transitional
                              (string-append
                               (xexpr->string
                                (html-page 
                                 #:title (title "Home")
                                 #:body ""))
                               (file->string
                                (build-path (find-doc-dir) "index.html"))))))
    #;(html-page 
     #:title (title "Home")
     #:body    
     `(div (h1 "Manuals")
           ,@(map (lambda (rep)
                    `(p (a ((href ,(format "~a/servlets/view.scm/~a/~a"
                                           base-url
                                           (repository-id rep)
                                           (repository-home-id rep))))
                           ,(repository-name rep))))
                  all-repositories)))
  )
  
  
  
      
      
      