
(module view mzscheme
  (require (lib "unitsig.ss")
           (prefix sb: "../syntax-browser/embed.ss")
           "gui.ss")
  (provide (all-defined))

  (define-values/invoke-unit/sig view^
    (compound-unit/sig
      (import)
      (link (PREFS : sb:prefs^ (sb:global-prefs@))
            (SB   : sb:implementation^ (sb:implementation@))
            (BASE : view-base^ (view-base@))
            (VIEW : view^ (view@ BASE PREFS SB)))
      (export (open VIEW))))
  )
