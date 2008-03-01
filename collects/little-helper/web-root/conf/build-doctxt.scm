#lang scheme

(require setup/dirs
         scribble/base-render
         (prefix-in text: scribble/text-render))

(define src-dir (build-path (find-collects-dir) "scribblings"))

(define (scrbl-file? path)
  (cond [(filename-extension path)
         => (λ (ext) (equal? ext #"scrbl"))]
        [else #f]))

(define current-render-mixin 
  (make-parameter text:render-mixin))
(define current-dest-directory
  (make-parameter (string->path "/tmp/")))
(define current-dest-name
  (make-parameter #f))
(define current-info-output-file
  (make-parameter #f))
(define current-info-input-files
  (make-parameter null))

(define (build-docs-files files)
  (build-docs (map (lambda (file)
                     (display file) (newline)
                     (dynamic-require `(file ,(path->string file)) 'doc))
                   files)
              files))

(define (build-docs docs files)
  (let ([dir (current-dest-directory)])
    (when dir 
      (make-directory* dir))
    
    (let ([renderer (new ((current-render-mixin) render%)
                         [dest-dir dir])])
      (send renderer report-output!)
      (let* ([fns (map (lambda (fn)
                         (let-values ([(base name dir?) (split-path fn)])
                           (let ([fn (path-replace-suffix (or (current-dest-name) name)
                                                          (send renderer get-suffix))])
                             (if dir
                                 (build-path dir fn)
                                 fn))))
                       files)]
             [info (send renderer collect docs fns)])
        (let ([info (let loop ([info info]
                               [files (reverse (current-info-input-files))])
                      (if (null? files)
                          info
                          (loop (let ([s (with-input-from-file (car files) read)])
                                  (send renderer deserialize-info s info)
                                  info)
                                (cdr files))))])
          (let ([r-info (send renderer resolve docs fns info)])   ; ERROR 
            (send renderer render docs fns r-info)
            (when (current-info-output-file)
              (let ([s (send renderer serialize-info r-info)])
                (with-output-to-file (current-info-output-file)
                  #:exists 'truncate/replace
                  (lambda ()
                    (write s)))))))))))

(define (scrbl-files src-dir)
  (let* ([sub-dirs              
          (directory-list src-dir)]
         [potential-scrbl-files 
          (map (λ (sub-dir)
                 (build-path src-dir sub-dir 
                             (string-append (path->string sub-dir) ".scrbl")))
               sub-dirs)])
    (filter file-exists? potential-scrbl-files)))

(build-docs-files (scrbl-files src-dir))
