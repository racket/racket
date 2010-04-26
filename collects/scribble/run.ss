#lang racket/base

(require "core.ss"
         "base-render.ss"
         "xref.ss"
         scheme/cmdline
         scheme/file
         scheme/class
         raco/command-name
         (prefix-in text:  "text-render.ss")
         (prefix-in html:  "html-render.ss")
         (prefix-in latex: "latex-render.ss")
         (prefix-in pdf:   "pdf-render.ss"))

(define multi-html:render-mixin
  (lambda (%) (html:render-multi-mixin (html:render-mixin %))))

(define current-render-mixin       (make-parameter text:render-mixin))
(define current-dest-directory     (make-parameter #f))
(define current-dest-name          (make-parameter #f))
(define current-info-output-file   (make-parameter #f))
(define current-info-input-files   (make-parameter null))
(define current-xref-input-modules (make-parameter null))
(define current-prefix-file        (make-parameter #f))
(define current-style-file         (make-parameter #f))
(define current-style-extra-files  (make-parameter null))
(define current-extra-files        (make-parameter null))
(define current-redirect           (make-parameter #f))
(define current-redirect-main      (make-parameter #f))

(define (read-one str)
  (let ([i (open-input-string str)])
    (with-handlers ([exn:fail:read? (lambda (x) #f)])
      (let ([v (read i)])
        (and (eof-object? (read i)) v)))))

(current-render-mixin html:render-mixin)

(define (run)
  (command-line
   #:program (short-program+command-name)
   #:once-any
   [("--text") "generate text-format output (the default)"
    (current-render-mixin text:render-mixin)]
   [("--html") "generate HTML-format output file"
    (current-render-mixin html:render-mixin)]
   [("--htmls") "generate HTML-format output directory"
    (current-render-mixin multi-html:render-mixin)]
   [("--latex") "generate LaTeX-format output"
    (current-render-mixin latex:render-mixin)]
   [("--pdf") "generate PDF-format output (with PDFLaTeX)"
    (current-render-mixin pdf:render-mixin)]
   #:once-each
   [("--dest") dir "write output in <dir>"
    (current-dest-directory dir)]
   [("--dest-name") name "write output as <name>"
    (current-dest-name name)]
   [("--prefix") file "use given .html/.tex prefix (for doctype/documentclass)"
    (current-prefix-file file)]
   [("--style") file "use given base .css/.tex file"
    (current-style-file file)]
   [("--redirect") url "redirect external links to tag search via <url>"
    (current-redirect url)]
   [("--redirect-main") url "redirect main doc links to <url>"
    (current-redirect-main url)]
   [("--info-out") file "write format-specific link information to <file>"
    (current-info-output-file file)]
   #:multi
   [("++extra") file "add given file"
    (current-extra-files (cons file (current-extra-files)))]
   [("++style") file "add given .css/.tex file after others"
    (current-style-extra-files (cons file (current-style-extra-files)))]
   [("++info-in") file "load format-specific link information from <file>"
    (current-info-input-files
     (cons file (current-info-input-files)))]
   [("++xref-in") module-path proc-id "load format-specific link information by"
    "calling <proc-id> as exported by <module-path>"
    (let ([mod (read-one module-path)]
          [id (read-one proc-id)])
      (unless (module-path? mod)
        (raise-user-error
         'scribble "bad module path for ++ref-in: ~s" module-path))
      (unless (symbol? id)
        (raise-user-error
         'scribble "bad procedure identifier for ++ref-in: ~s" proc-id))
      (current-xref-input-modules
       (cons (cons mod id) (current-xref-input-modules))))]
   #:args (file . another-file)
   (let ([files (cons file another-file)])
     (build-docs (map (lambda (file) (dynamic-require `(file ,file) 'doc))
                      files)
                 files))))

(define (build-docs docs files)
  (define dir (current-dest-directory))
  (when dir (make-directory* dir))
  (let ([renderer (new ((current-render-mixin) render%)
                    [dest-dir dir]
                    [prefix-file (current-prefix-file)]
                    [style-file (current-style-file)]
                    [style-extra-files (reverse (current-style-extra-files))]
                    [extra-files (reverse (current-extra-files))])])
    (when (current-redirect)
      (send renderer set-external-tag-path (current-redirect)))
    (when (current-redirect-main)
      (send renderer set-external-root-url (current-redirect-main)))
    (send renderer report-output!)
    (let* ([fns (map (lambda (fn)
                       (let-values ([(base name dir?) (split-path fn)])
                         (let ([fn (path-replace-suffix
                                    (or (current-dest-name) name)
                                    (send renderer get-suffix))])
                           (if dir (build-path dir fn) fn))))
                     files)]
           [info (send renderer collect docs fns)])
      (for ([file (in-list (reverse (current-info-input-files)))])
        (let ([s (with-input-from-file file read)])
          (send renderer deserialize-info s info)))
      (for ([mod+id (in-list (reverse (current-xref-input-modules)))])
        (let* ([get-xref (dynamic-require (car mod+id) (cdr mod+id))]
               [xr (get-xref)])
          (unless (xref? xr)
            (raise-user-error
             'scribble "result from `~s' of `~s' is not an xref: ~e"
             (cdr mod+id) (car mod+id) xr))
          (xref-transfer-info renderer info xr)))
      (let ([r-info (send renderer resolve docs fns info)])
        (send renderer render docs fns r-info)
        (when (current-info-output-file)
          (let ([s (send renderer serialize-info r-info)])
            (with-output-to-file (current-info-output-file)
              #:exists 'truncate/replace
              (lambda () (write s)))))))))

(run)
