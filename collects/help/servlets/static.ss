;; Serve static documentation.
;; A search bar is added on top of the screen, when an external browser is used.
;; (which is why we don't let the web-server serve the documentation directly)

(module static mzscheme
  (require (lib "private/mime-types.ss" "web-server")
           (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "match.ss")
           (lib "url.ss" "net")
           (lib "dirs.ss" "setup")
           (lib "port.ss")
           "../private/standard-urls.ss"
           "../private/docpos.ss"
           "../private/options.ss"
           "private/html.ss")
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  ;;;
  ;;; PORT UTILS
  ;;;
  
  (define (port->string port)
    (let ([os (open-output-string)])
      (copy-port port os)
      (get-output-string os)))
  
  (define (file->string path)
    (call-with-input-file path
      port->string))
  
  (define (port->bytes port)
    (let ([ob (open-output-bytes)])
      (copy-port port ob)
      (get-output-bytes ob)))
  
  (define (file->bytes path)
    (call-with-input-file path
      port->bytes))
  
  ;;;
  ;;; MIME
  ;;;
  
  ; get-mime-type : path -> string
  (define get-mime-type 
    (;make-get-mime-type 
     make-path->mime-type
     (build-path (find-collects-dir) 
                 "web-server" "default-web-root" "mime.types")))
  
  
  (define (text-mime-type? file-path)
    (regexp-match #rx"^text"
                  (get-mime-type file-path)))
  
  ;;;
  ;;; URL
  ;;;
  
  ; file-parts->file : string (list string) -> string
  ;   (list "foo" "bar" "baz") => "foo/bar/baz"
  (define (file-parts->file manual fs)
    (apply string-append
           (let loop ([fs (cons manual fs)])
             (cond
               [(null? fs)       (list "")]
               [(null? (cdr fs)) (list (car fs))]
               [else             (cons (string-append (car fs) "/")
                                       (loop (cdr fs)))]))))
  
  ;;;
  ;;; TITLES
  ;;;
  
  (define (short->manual-title s)
    (match (assoc (string->path s) known-docs)
      [#f             "Documentation"]
      [(path . long)  long]))
  
  (define (start request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       (let* ([bindings  (request-bindings request)]
              [file      (get-binding bindings 'file "no file")]
              [host      (get-binding bindings 'host "no host")]
              
              [url       (request-uri request)])
         (let-values
             ([(file-path host manual)
               (match (map path/param-path (url-path url))
                 [("servlets" "static.ss" host manual . file-parts)
                  (values (host+file->path host (file-parts->file manual file-parts))
                          host 
                          manual)])])
           (cond
             [(not file-path)
              (list #"text/html"
                    "<html><head><title>Not found</title></head><body>File not found.</body></html>")]
             [(and (file-exists? file-path)
                   (text-mime-type? file-path))
              (list (get-mime-type file-path)
                    (string-append (xexpr->string
                                    (html-page 
                                     #:title (short->manual-title manual)
                                     #:top (case (helpdesk-platform)
                                             [(internal-browser) '()]
                                             [(internal-browser-simple) '()]
                                             [else               (html-top request)])
                                     #:body " "))
                                   (file->string file-path)))]
             [(file-exists? file-path)
              (list (get-mime-type file-path)
                    (file->bytes file-path))]
             [else
              (list #"text/html"
                    (format "<html><head><title>Not found</title></head><body>File not found: ~a</body></html>"
                            file-path))]))))))
  
  )
