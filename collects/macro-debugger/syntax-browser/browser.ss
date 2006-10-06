
(module browser mzscheme
  (require (lib "unitsig.ss")
           "interfaces.ss"
           "frame.ss"
           "implementation.ss")
  (provide-signature-elements browser^)
  (provide-signature-elements snip^)
  
  (define browser@
    (compound-unit/sig 
      (import)
      (link [PREFS : prefs^          (global-prefs@)]
            [IMPL  : implementation^ (implementation@)]
            [FRAME : browser^        (frame@ PREFS (IMPL widget))])
      (export (open FRAME))))
  
  (define-values/invoke-unit/sig browser^ browser@)
  )
