
(module implementation mzscheme
  (require (lib "unit.ss")
           "interfaces.ss"
           "widget.ss"
           "syntax-snip.ss"
           "snipclass.ss"
           "keymap.ss"
           "prefs.ss")
  (provide global-prefs@
           global-snip@
           widget-keymap@
           widget-context-menu@
           implementation@)
  (provide-signature-elements snip^)
  (provide-signature-elements snipclass^)

  ;; prefs@ and snip@ should only be invoked once
  ;; We create a new unit/sig out of their invocation

  (define snip-keymap@
    (compound-unit
      (import [MENU : context-menu^]
              [SNIP : snip^])
      (link [((KEYMAP : keymap^))      keymap@ MENU SNIP]
            [((SNIP-KEYMAP : keymap^)) snip-keymap-extension@ KEYMAP])
      (export SNIP-KEYMAP)))
  
  (define snip-implementation@
    (compound-unit
      (import)
      (link [((PREFS : prefs^)) prefs@]
            [((MENU : context-menu^)) context-menu@]
            [((KEYMAP : keymap^)) snip-keymap@ MENU SNIP]
            [((SNIP-CLASS : snipclass^)) snipclass@ SNIP]
            [((SNIP : snip^)) snip@ PREFS KEYMAP MENU SNIP-CLASS])
      (export PREFS SNIP SNIP-CLASS)))
  (define-values/invoke-unit snip-implementation@
    (import)
    (export snip^ prefs^ snipclass^))

  (define global-prefs@ (unit-from-context prefs^))

  (define global-snip@ (unit-from-context snip^))

  ;; Everyone else re-uses the global-snip@ unit
  
  (define widget-keymap@
    (compound-unit
      (import [MENU : context-menu^]
              [SNIP : snip^])
      (link [((KEYMAP : keymap^)) keymap@ MENU SNIP]
            [((WKEYMAP : keymap^)) widget-keymap-extension@ KEYMAP])
      (export WKEYMAP)))

  (define widget-context-menu@
    (compound-unit
      (import)
      (link [((MENU : context-menu^)) context-menu@]
            [((WMENU : context-menu^)) widget-context-menu-extension@ MENU])
      (export WMENU)))
  
  ;; implementation@ : snip^ widget^
  (define implementation@
    (compound-unit
      (import)
      (link [((SNIP : snip^)) global-snip@]
            [((MENU : context-menu^)) widget-context-menu@]
            [((KEYMAP : keymap^)) widget-keymap@ MENU SNIP]
            [((WIDGET : widget^)) widget@ KEYMAP])
      (export SNIP WIDGET)))

  )
