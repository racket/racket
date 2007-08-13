
(module pdf mzscheme
  (require (lib "run.ss" "scribble")
           (prefix guide: "guide/guide.scrbl")
           (prefix reference: "reference/reference.scrbl")
           (prefix quick: "quick/quick.scrbl")
           (prefix scribble: "scribble/scribble.scrbl")
           (prefix gui: "gui/gui.scrbl")
           (lib "process.ss"))

  (define docs (list guide:doc
                     reference:doc
                     quick:doc
                     scribble:doc
                     gui:doc))
  (define names (list "guide"
                      "reference"
                      "quick"
                      "scribble"
                      "gui"))

  (define temp-dir (find-system-path 'temp-dir))

  (parameterize ([current-dest-directory temp-dir]
                 [current-render-mixin latex:render-mixin]
                 [current-directory (collection-path "scribblings")])
    (build-docs docs names))

  (parameterize ([current-directory temp-dir])
    (for-each (lambda (name)
                (unless (system (format "pdflatex ~a && pdflatex ~a && pdflatex ~a"
                                        name
                                        name
                                        name))
                  (error "stopped")))
              names))

  (for-each (lambda (name)
              (let ([pdf (path-replace-suffix name #".pdf")])
                (rename-file-or-directory (build-path temp-dir pdf)
                                          (build-path temp-dir "tmp.pdf")
                                          #t)
                (when (file-exists? pdf)
                  (delete-file pdf))
                (copy-file (build-path temp-dir "tmp.pdf") pdf)))
            names))


  
  
  