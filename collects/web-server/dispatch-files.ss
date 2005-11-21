(module dispatch-files mzscheme
  (require (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "list.ss"))
  (require "dispatch.ss"
           "util.ss"
           "response.ss")  
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define (gen-dispatcher htdocs-path indices file-not-found-responder)
    (lambda (conn req)
      (let-values ([(uri method path) (decompose-request req)])
        ;; ************************************************************
        ;; ************************************************************
        ;; SERVING FILES
        
        ;; serve-file : connection symbol uri host -> void
        ;; to find the file, including searching for implicit index files, and serve it out
        (let ([path (url-path->path htdocs-path
                                    (translate-escapes (url-path->string (url-path uri))))])
          (cond
            [(file-exists? path)
             (output-file conn path method (get-mime-type path))]
            [(directory-exists? path)
             (let loop ([dir-defaults indices])
               (cond
                 [(pair? dir-defaults)
                  (let ([full-name (build-path path (first dir-defaults))])
                    (if (file-exists? full-name)
                        (cond
                          [(looks-like-directory? (url-path->string (url-path uri)))
                           (output-file conn full-name method (get-mime-type full-name))]
                          [else
                           (output-slash-message conn method (url-path->string (url-path uri)))])
                        (loop (rest dir-defaults))))]
                 [else
                  (output-response/method conn (file-not-found-responder uri) method)]))]
            [else
             (output-response/method conn (file-not-found-responder uri) method)])))))
  
  ;; looks-like-directory : str -> bool
  ;; to determine if is url style path looks like it refers to a directory
  (define (looks-like-directory? path)
    (eq? #\/ (string-ref path (sub1 (string-length path)))))
  
  ;; output-slash-message: connection symbol string -> void
  ;; basically this is just a special error response
  (define (output-slash-message conn method url-path-str)
    (output-response/method
     conn
     (make-response/full
      301 "Moved Permanently"
      (current-seconds)
      TEXT/HTML-MIME-TYPE
      `([Location . ,(string-append url-path-str "/")])
      (list
       (xexpr->string
        `(html
          (head (title "Add a Slash"))
          (body "Please use "
                (a ([href ,(string-append
                            url-path-str "/")])
                   "this url") " instead.")))))
     method)))