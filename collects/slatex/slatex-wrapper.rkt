(module slatex-wrapper scheme/base
  (require mzlib/file
           scheme/contract
           mzlib/process
           mzlib/sendevent
           scheme/runtime-path
           "private/slatex.rkt")
  
  (define-runtime-path here ".")

  (provide/contract
   [slatex (string? . -> . boolean?)]
   [pdf-slatex (string? . -> . boolean?)]
   [slatex/no-latex (string? . -> . void?)]
   [latex (string? . -> . boolean?)]
   [pdf-latex (string? . -> . boolean?)]
   [filename->latex-filename (string? . -> . string?)])
  
  (define (add-suffix p s)
    (path->string
     (bytes->path 
      (bytes-append 
       (path->bytes (if (string? p) (string->path p) p)) s))))

  (define (filename->latex-filename input-file)
    (let* ([norm (normalize-path input-file)])
      (cond
        [(file-exists? norm) input-file]
        [(file-exists? (add-suffix norm #".tex"))
	 (add-suffix input-file #".tex")]
        [else
         (error 'filename->latex-filename "~e does not exist" input-file)])))

  (define (exec-latex exe file)
    (let ([latex-path (find-executable-path exe #f)])
      (unless latex-path
        (error 'latex "could not find latex binary: ~e" exe))
      (system* latex-path file)))

  ;; latex, pdf-latex : string -> boolean
  ;; boolean result indicates success
  (define-values (latex pdf-latex)
    (letrec ([meta-latex 
              (lambda (pdf?)
                (lambda (input-file)
                  (let ([file (filename->latex-filename input-file)]
                        [command-name (if pdf? 
                                          "pdflatex"
                                          "latex")])
                    (case (system-type)
                      [(macos)
                       (when pdf?
                         (error 'latex "do not know how to run pdflatex on ~s" (system-type)))
                       
                       (system "OTEX")
                       
                       ;; boy, wouldn't it be great if the "actv" appleevent worked for OTEX?
                       ;;(send-event "OTEX" "misc" "acvt")
                       (let* ([build-oztex-locations
                               (list
                                (lambda (x)
                                  (build-path x
                                              "Applications"
                                              "OzTeX"
                                              "OzTeX"))
                                (lambda (x)
                                  (build-path x
                                              "Applications (Mac OS 9)"
                                              "OzTeX"
                                              "OzTeX")))]
                              [oztex-locations
                               (apply
                                append
                                (map (lambda (f) (map f (filesystem-root-list))) build-oztex-locations))]
                              [oztex-location (ormap (lambda (x) (if (file-exists? x) x #f)) oztex-locations)])
                         (when oztex-location
                           (with-handlers ([void void]) ;; mzscheme cannot handle result
                             (send-event "MACS" "aevt" "odoc" (vector 'file oztex-location)))))
                       (send-event "OTEX" "aevt" "odoc" (vector 'file file))
                       #t]
                      [(windows) (exec-latex (add-suffix command-name #".exe") file)]
                      [(unix macosx)
                       (exec-latex command-name file)]
                      [else
                       (error 'latex "do not know how to run ~s on ~s" command-name (system-type))]))))])
      (values
       (meta-latex #f)
       (meta-latex #t))))
  
  (define-values (slatex pdf-slatex)
    (letrec ([meta-slatex
              (lambda (latex-fun)
                (lambda (filename)
                  (slatex/no-latex filename)
                  (putenv "TEXINPUTS" 
                          (format "~a:~a" 
                                  (path->string here)
                                  (or (getenv "TEXINPUTS") "")))
                  (latex-fun filename)))])
      (values 
       (meta-slatex latex)
       (meta-slatex pdf-latex))))

  (define (slatex/no-latex input-file)
    (let* ([fixed-file (filename->latex-filename input-file)]
           [file (normalize-path fixed-file)])
      (let-values ([(base name dir?) (split-path file)])
        (parameterize ([current-directory
                        (if (string? base)
                            base
                            (current-directory))])
          (slatex::process-main-tex-file (path->string name)))))))

