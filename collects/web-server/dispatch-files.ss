(module dispatch-files mzscheme
  (require (lib "url.ss" "net")
           (lib "xml.ss" "xml"))
  (require "dispatch.ss"
           "util.ss"
           "configuration-structures.ss"
           "response.ss")  
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define (gen-dispatcher host-info)
    (lambda (conn req)
      (let-values ([(uri method path) (decompose-request req)])
        (serve-file conn method uri host-info))))
  
  ;; ************************************************************
  ;; ************************************************************
  ;; SERVING FILES
  
  ;; serve-file : connection symbol uri host -> void
  ;; to find the file, including searching for implicit index files, and serve it out
  (define (serve-file conn method uri host-info)
    (let ([path (url-path->path (paths-htdocs (host-paths host-info))
                                (translate-escapes (url-path->string (url-path uri))))])
      (cond
        [(file-exists? path)
         (output-file conn path method (get-mime-type path))]
        [(directory-exists? path)
         (let loop ([dir-defaults (host-indices host-info)])
           (cond
             [(pair? dir-defaults)
              (let ([full-name (build-path path (car dir-defaults))])
                (if (file-exists? full-name)
                    (cond
                      [(looks-like-directory? (url-path->string (url-path uri)))
                       (output-file conn full-name method (get-mime-type full-name))]
                      [else
                       (output-slash-message conn method (url-path->string (url-path uri)))])
                    (loop (cdr dir-defaults))))]
             [else
              (output-response/method
               conn
               ((responders-file-not-found
                 (host-responders host-info)) uri)
               method)]))]
        [else
         (output-response/method
          conn ((responders-file-not-found (host-responders host-info))
                uri)
          method)])))
  
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