(module doc-installer mzscheme
  (require (lib "dirs.ss" "setup")
           (prefix core: "core.ss")
           (prefix quick: "quick.ss")
           (prefix scribble: "scribble.ss"))

  (provide post-installer)

  (define post-installer
    (lambda (path)
      (let ([doc (find-doc-dir)])
        (when doc
          (core:build)
          (quick:build)
          (scribble:build))))))
