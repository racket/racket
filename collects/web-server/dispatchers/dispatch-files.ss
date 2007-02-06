(module dispatch-files mzscheme
  (require (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "kw.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "uri-codec.ss" "net"))
  (require (lib "pretty.ss"))         
  (require "dispatch.ss"
           "../private/configuration.ss"
           "../private/util.ss"
           "../private/mime-types.ss"
           "../private/request.ss"
           "../private/servlet-helpers.ss"
           "../private/response.ss"
           "../response-structs.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide ; XXX contract kw
   make)
  
  ; more here - ".." should probably raise an error instead of disappearing.
  (define (url-path->path base p)
    (path->complete-path
     (apply build-path base
            (reverse!
             (foldl (lambda (x acc)
                      (cond
                        [(string=? x "") acc]
                        [(string=? x ".") acc]
                        [(string=? x "..") (if (pair? acc) (cdr acc) acc)]
                        [else (cons x acc)]))
                    null
                    (regexp-split #rx"/" p))))))
  
  (define interface-version 'v1)
  (define/kw (make #:key 
                   [htdocs-path "htdocs"]
                   [mime-types-path "mime.types"]
                   [indices (list "index.html" "index.htm")]
                   [file-not-found-responder 
                    (gen-file-not-found-responder "not-found.html")])
    (define get-mime-type (make-get-mime-type mime-types-path))
    (lambda (conn req)
      (define-values (uri method _path) (decompose-request req))
      ;; ************************************************************
      ;; ************************************************************
      ;; SERVING FILES
      
      ;; serve-file : connection symbol uri host -> void
      ;; to find the file, including searching for implicit index files, and serve it out
      (define path 
        (url-path->path htdocs-path
                        (uri-decode (url-path->string (url-path uri)))))
      (cond
        [(file-exists? path)
         (match (headers-assq* #"Range" (request-headers/raw req))
           [#f
            (output-file conn path method (get-mime-type path))]
           [range
            (match (bytes->string/utf-8 (header-value range))
              [(regexp "^bytes=(.*)-(.*)$" (list s start end))
               (define startn
                 (if (string=? "" start)
                     0
                     (string->number start)))
               (define endn
                 (if (string=? "" end)
                     +inf.0
                     (string->number end)))               
               (output-file/partial conn path method (get-mime-type path)
                                    startn endn)]
              [r
               ; XXX: Unhandled range: r
               (output-file conn path method (get-mime-type path))])])]
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
         (output-response/method conn (file-not-found-responder uri) method)])))
  
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