(module main-doc mzscheme
  (require "dirs.ss"
           "path-relativize.ss")

  (provide path->main-doc-relative
           main-doc-relative->path)

  (define-values (path->main-doc-relative
                  main-doc-relative->path)
    (make-relativize find-doc-dir 'doc
                     'path->main-doc-relative
                     'main-doc-relative->path)))

