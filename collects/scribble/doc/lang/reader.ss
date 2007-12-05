(module reader mzscheme
  (require (prefix doc: (lib "reader.ss" "scribble" "doc")))
  (provide (rename doc:read read)
           (rename doc:read-syntax read-syntax)))
