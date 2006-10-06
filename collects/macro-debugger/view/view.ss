
(module view mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix sb: "../syntax-browser/embed.ss")
           "interfaces.ss"
           "prefs.ss"
           "gui.ss")
  (provide (all-defined))

  (define view-base@
    (unit/sig view-base^
      (import)
      (define base-frame%
        (frame:standard-menus-mixin (frame:basic-mixin frame%)))))

  (define-values/invoke-unit/sig view^
    (compound-unit/sig
      (import)
      (link (PREFS : prefs^ (prefs@))
            (SB   : sb:implementation^ (sb:implementation@))
            (BASE : view-base^ (view-base@))
            (VIEW : view^ (view@ BASE SB)))
      (export (open VIEW))))
  )
