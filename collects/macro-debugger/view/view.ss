
(module view mzscheme
  (require (lib "unitsig.ss"))
  (require "gui.ss")
  (provide (all-defined))

  (define-values/invoke-unit/sig view^
    (compound-unit/sig
      (import)
      (link (BASE : view-base^ (view-base@))
            (VIEW : view^ (view@ BASE)))
      (export (open VIEW))))
  )