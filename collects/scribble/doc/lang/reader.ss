(module reader mzscheme
  (require (prefix doc: (lib "docreader.ss" "scribble")))
  (provide (rename doc:read read)
           (rename doc:read-syntax read-syntax)))
