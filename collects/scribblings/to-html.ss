
(module to-html mzscheme
  (require (lib "dirs.ss" "setup")
           (lib "run.ss" "scribble")
           (lib "file.ss"))

  (provide to-html)
  
  (define (to-html multi? core? docs names)
    (let ([main-doc-dir (find-doc-dir)]
          [dir (collection-path "scribblings")])
      (unless main-doc-dir
        (error "cannot find installation doc directory"))
      (let ([doc-dir (if multi?
                         main-doc-dir
                         (build-path main-doc-dir (car names)))])
        (unless multi?
          (make-directory* doc-dir))
        (when multi?
          (for-each (lambda (name)
                      (let ([out-dir (build-path doc-dir name)])
                        (when (directory-exists? out-dir)
                          (delete-directory/files out-dir))))
                    names))
        (parameterize ([current-directory dir]
                       [current-dest-directory doc-dir]
                       [current-render-mixin (if multi?
                                                 multi-html:render-mixin
                                                 html:render-mixin)]
                       [current-info-output-file (if core?
                                                     (build-path main-doc-dir
                                                                 "reference"
                                                                 "core-info-html.data")
                                                     (current-info-output-file))]
                       [current-info-input-files (append
                                                  (if core?
                                                      null
                                                      (list (build-path main-doc-dir
                                                                        "reference"
                                                                        "core-info-html.data")))
                                                  (current-info-input-files))])
          (build-docs docs (if multi?
                               names
                               (list "index"))))))))

