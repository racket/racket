
(module browser mzscheme
  (require (lib "unit.ss")
           "interfaces.ss"
           "frame.ss"
           "implementation.ss")
  (provide-signature-elements browser^)
  (provide-signature-elements snip^)
  
  (define browser@
    (compound-unit
      (import)
      (link [((PREFS : prefs^))    global-prefs@]
            [((WIDGET : widget^)) implementation@]
            [((FRAME : browser^))  frame@ PREFS WIDGET])
      (export FRAME)))

  (define-values/invoke-unit browser@
    (import)
    (export browser^))
  )
