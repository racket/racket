(module ppretty-prints mzscheme

  (provide pp)

  (require (lib "pretty.ss"))
  (define pp pretty-print))
