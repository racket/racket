(module mime mzscheme
  (provide (all-defined))

  (require (lib "private/mime-types.ss" "web-server")
           (lib "dirs.ss" "setup")
           (lib "port.ss")
           "../../private/docpos.ss")
  
  ;;;
  ;;; MIME
  ;;;
  
  ; get-mime-type : path -> string
  (define get-mime-type 
    (let ([path->mime-type
           (make-path->mime-type
            (build-path (find-collects-dir) 
                        "web-server" "default-web-root" "mime.types"))])
      (lambda (file)
        (path->mime-type
         (if (string? file)
             (string->path file)
             file)))))
  
  
  (define (text-mime-type? file-path)
    (regexp-match #rx"^text"
                  (get-mime-type file-path)))
  
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

  
  )
