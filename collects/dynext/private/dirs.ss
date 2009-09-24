
(module dirs mzscheme
  (require setup/dirs)

  (define include-dir find-include-dir)
  (define std-library-dir find-lib-dir)

  (provide include-dir std-library-dir))
