(module dispatch-files mzscheme
  (require (lib "url.ss" "net")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/util.ss"
           "../private/mime-types.ss"
           "../private/request-structs.ss"
           "../private/response.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
    
  ;; looks-like-directory : str -> bool
  ;; to determine if is url style path looks like it refers to a directory
  (define (looks-like-directory? path)
    (eq? #\/ (string-ref path (sub1 (string-length path)))))
  
  (define interface-version 'v1)
  (define/kw (make #:key 
                   url->path
                   ; XXX Make the default a define from response.ss
                   [path->mime-type (lambda (path) #"text/plain; charset=utf-8")]
                   [indices (list "index.html" "index.htm")])
    (lambda (conn req)
      (define uri (request-uri req))
      (define method (request-method req))
      (define-values (path _) (url->path uri))
      (cond
        [(file-exists? path)
         (match (headers-assq* #"Range" (request-headers/raw req))
           [#f
            (output-file conn path method (path->mime-type path)
                         0 +inf.0)]
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
               (output-file conn path method (path->mime-type path)
                                    startn endn)]
              [r
               ; XXX: Unhandled range: r
               (output-file conn path method (path->mime-type path)
                            0 +inf.0)])])]
        [(and (directory-exists? path)
              (looks-like-directory? (url-path->string (url-path uri))))
         (let/ec esc
           (for-each (lambda (dir-default)
                       (define full-name (build-path path dir-default))
                       (when (file-exists? full-name)
                         (esc (output-file conn full-name method (path->mime-type full-name)
                                           0 +inf.0))))
                     indices)
           (next-dispatcher))]
        [else
         (next-dispatcher)]))))