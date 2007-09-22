(module reader mzscheme
  (require (lib "module-reader.ss" "syntax"))

  (provide-module-reader (lib "main.ss" "mred" "lang")))
