
(module embed-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide compiler:embed^)

  (define-signature compiler:embed^
    (make-embedding-executable
     embedding-executable-is-directory?
     embedding-executable-put-file-extension+style+filters)))
