(module read-doc mzscheme
  (require (lib "etc.ss")
           (lib "getinfo.ss" "setup")
           (lib "xml.ss" "xml")
           "../../private/options.ss"
           "util.ss"
           "read-lines.ss"
           "html.ss"
           "mime.ss")
  (provide read-doc)

  ;; extracts help desk message
  (define (get-message coll)
    (with-handlers ([void (lambda _ #f)]) ; collection may not exist
      ((get-info (list coll)) 'help-desk-message (lambda () #f))))
  
  (define offset-format "file=~a&caption=~a&offset=~a#temp")

  (define (build-page request file-path caption coll offset)
    (html-page 
     #:title (if (string? caption) caption "Documentation")
     #:top (case (helpdesk-platform)
             [(internal-browser internal-browser-simple) '()]
             [else (html-top request)])
     #:body 
     (let ([msg (get-message coll)])
       (cond
         [(not file-path)
          (format "File not found.")]
        [(file-exists? file-path)
         (if msg 
             `(div (p ,msg) ,(read-lines file-path caption offset))
             (read-lines file-path caption offset))]
        [else
         (format "File not found: ~a" file-path)]))))

  (define read-doc
    (opt-lambda (request file caption coll [offset #f])
      (build-page request file caption coll offset))))
