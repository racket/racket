(module main-collects mzscheme
  (require "dirs.ss"
           "path-relativize.ss")

  (provide path->main-collects-relative
           main-collects-relative->path)

  (define-values (path->main-collects-relative
                  main-collects-relative->path)
    (make-relativize find-collects-dir 'collects
                     'path->main-collects-relative
                     'main-collects-relative->path)))

