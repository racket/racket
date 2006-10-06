
(module implementation mzscheme
  (require (lib "unitsig.ss")
           "interfaces.ss"
           "widget.ss"
           "syntax-snip.ss"
           "snipclass.ss"
           "keymap.ss"
           "prefs.ss")
  (provide global-prefs@
           global-snip@
           implementation@)
  (provide-signature-elements snip^)
  (provide-signature-elements snipclass^)

  ;; prefs@ and snip@ should only be invoked once
  ;; We create a new unit/sig out of their invocation

  (define snip-implementation@
    (compound-unit/sig 
      (import)
      (link [PREFS      : prefs^ (prefs@)]
            [KEYMAP     : keymap^ (keymap@)]
            [MENU       : context-menu^ (context-menu@ SNIP)]
            [SNIP-CLASS : snipclass^ (snipclass@ SNIP)]
            [SNIP-MENU  : context-menu^ (snip-context-menu-extension@ MENU)]
            [SNIP       : snip^ (snip@ PREFS KEYMAP SNIP-MENU SNIP-CLASS)])
      (export (open PREFS) (open SNIP) (open SNIP-CLASS))))
  (define-values/invoke-unit/sig ((open snip^) (open prefs^) (open snipclass^))
    snip-implementation@)
  
  (define global-prefs@
    (unit/sig prefs^
      (import)
      (rename (-width pref:width)
              (-height pref:height)
              (-props-percentage pref:props-percentage))
      (define -width pref:width)
      (define -height pref:height)
      (define -props-percentage pref:props-percentage)))
  
  (define global-snip@
    (unit/sig snip^
      (import)
      (rename (-syntax-snip syntax-snip)
              (-syntax-snip% syntax-snip%))
      (define -syntax-snip syntax-snip)
      (define -syntax-snip% syntax-snip%)))

  ;; Everyone else re-uses the global-snip@ unit
  
  ;; implementation@ : prefs^ -> implementation^
  (define implementation@
    (compound-unit/sig
      (import)
      (link [KEYMAP        : keymap^       (keymap@)]
            [MENU          : context-menu^ (context-menu@ SNIP)]
            [SNIP          : snip^         (global-snip@)]
            [WIDGET-MENU   : context-menu^ (widget-context-menu-extension@ MENU)]
            [WIDGET        : widget^       (widget@ KEYMAP WIDGET-MENU)])
      (export (unit SNIP snip)
              (unit WIDGET widget))))

  )
