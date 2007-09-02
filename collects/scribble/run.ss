
(module run mzscheme
  (require "struct.ss"
           "base-render.ss"
           (lib "cmdline.ss")
           (lib "class.ss")
           (lib "file.ss")
           (prefix text: "text-render.ss")
           (prefix html: "html-render.ss")
           (prefix latex: "latex-render.ss"))

  (provide (all-defined)
           html:render-mixin
           latex:render-mixin
           text:render-mixin)

  (define multi-html:render-mixin
    (lambda (%)
      (html:render-multi-mixin
       (html:render-mixin %))))

  (define current-render-mixin 
    (make-parameter text:render-mixin))
  (define current-dest-directory
    (make-parameter #f))
  (define current-dest-name
    (make-parameter #f))
  (define current-info-output-file
    (make-parameter #f))
  (define current-info-input-files
    (make-parameter null))

  (define (get-command-line-files argv)
    (command-line
     "scribble"
     argv
     [once-any
      [("--text") "generate text-format output (the default)"
       (void)]
      [("--html") "generate HTML-format output file"
       (current-render-mixin html:render-mixin)]
      [("--htmls") "generate HTML-format output directory"
       (current-render-mixin multi-html:render-mixin)]
      [("--latex") "generate LaTeX-format output"
       (current-render-mixin latex:render-mixin)]]
     [once-each
      [("--dest") dir "write output in <dir>"
       (current-dest-directory dir)]
      [("--dest-name") name "write output as <name>"
       (current-dest-name name)]
      [("--info-out") file "write format-specific link information to <file>"
       (current-info-output-file file)]]
     [multi
      [("++info-in") file "load format-specific link information form <file>"
       (current-info-input-files
        (cons file (current-info-input-files)))]]
     [args file file]))

  (define (build-docs-files files)
    (build-docs (map (lambda (file)
                       (dynamic-require file 'doc))
                     files)
                files))
    
  (define (build-docs docs files)
    (let ([dir (current-dest-directory)])
      (when dir 
        (make-directory* dir))

      (let ([renderer (new ((current-render-mixin) render%)
                           [dest-dir dir])])
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
            (let ([r-info (send renderer resolve docs fns info)])
              (send renderer render docs fns r-info)
              (when (current-info-output-file)
                (let ([s (send renderer serialize-info r-info)])
                  (with-output-to-file (current-info-output-file)
                    (lambda ()
                      (write s))
                    'truncate/replace))))))))))
