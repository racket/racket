(module mime mzscheme
  (provide get-mime-type
           text-mime-type?)
  
  (require (lib "dirs.ss" "setup")
           (lib "private/mime-types.ss" "web-server"))
  
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
                  (get-mime-type file-path))))