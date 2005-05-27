
(module slatex-wrapper mzscheme
  (require (lib "file.ss")
	   (lib "process.ss")
	   (lib "sendevent.ss")
           "slatex.ss")

  (provide slatex latex pdf-slatex pdf-latex slatex/no-latex)

  (define (add-suffix p s)
    (path->string
     (bytes->path 
      (bytes-append 
       (path->bytes (if (string? p) (string->path p) p)) s))))

  (define (filename->latex-filename input-file)
    (let ([norm (normalize-path input-file)])
      (cond
        [(file-exists? norm) input-file]
        [(file-exists? (add-suffix norm #".tex"))
	 (add-suffix input-file #".tex")]
        [else
         (error 'filename->latex-filename "~e does not exist" input-file)])))

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
                      [(windows unix macosx) ;; is this also okay for beos?
		       (let ([latex-path (find-executable-path command-name #f)])
			 (unless latex-path
			   (error 'latex "could not find latex binary"))
			 (system* latex-path file))]
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

