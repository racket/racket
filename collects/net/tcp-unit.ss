(module tcp-unit mzscheme
  (provide tcp@)

  (require (lib "unit.ss") "tcp-sig.ss")

  (define-unit-from-context tcp@ tcp^))
