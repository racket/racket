(module dispatch-files mzscheme
  (require (lib "url.ss" "net")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../configuration/responders.ss"
           "../private/util.ss"
           "../private/mime-types.ss"
           "../request-structs.ss"
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
                   [mime-types-path "mime.types"]
                   [indices (list "index.html" "index.htm")])
    (define get-mime-type (make-get-mime-type mime-types-path))
    (lambda (conn req)
      (define uri (request-uri req))
      (define method (request-method req))
      (define-values (path _) (url->path uri))
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
         (let/ec esc
           (for-each (lambda (dir-default)
                       (define full-name (build-path path dir-default))
                       (when (and (file-exists? full-name)
                                  (looks-like-directory? (url-path->string (url-path uri))))
                         (esc (output-file conn full-name method (get-mime-type full-name)))))
                     indices)
           (next-dispatcher))]
        [else
         (next-dispatcher)]))))