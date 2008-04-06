(module reader mzscheme
  (require (prefix doc: scribble/doc/reader))
  (provide (rename doc:read read)
           (rename doc:read-syntax read-syntax)))
