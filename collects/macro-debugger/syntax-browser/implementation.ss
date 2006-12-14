
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
           widget-keymap@
           widget-context-menu@
           implementation@)
  (provide-signature-elements snip^)
  (provide-signature-elements snipclass^)

  ;; prefs@ and snip@ should only be invoked once
  ;; We create a new unit/sig out of their invocation

  (define snip-keymap@
    (compound-unit/sig
      (import [MENU : context-menu^]
              [SNIP : snip^])
      (link [KEYMAP : keymap^ (keymap@ MENU SNIP)]
            [SNIP-KEYMAP : keymap^ (snip-keymap-extension@ KEYMAP)])
      (export (open SNIP-KEYMAP))))
  
  (define snip-implementation@
    (compound-unit/sig 
      (import)
      (link [PREFS       : prefs^ (prefs@)]
            [MENU        : context-menu^ (context-menu@)]
            [KEYMAP      : keymap^ (snip-keymap@ MENU SNIP)]
            [SNIP-CLASS  : snipclass^ (snipclass@ SNIP)]
            [SNIP        : snip^ (snip@ PREFS KEYMAP MENU SNIP-CLASS)])
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
  
  (define widget-keymap@
    (compound-unit/sig
      (import [MENU : context-menu^]
              [SNIP : snip^])
      (link [KEYMAP : keymap^ (keymap@ MENU SNIP)]
            [WKEYMAP : keymap^ (widget-keymap-extension@ KEYMAP)])
      (export (open WKEYMAP))))
  
  (define widget-context-menu@
    (compound-unit/sig
      (import)
      (link [MENU : context-menu^ (context-menu@)]
            [WMENU : context-menu^ (widget-context-menu-extension@ MENU)])
      (export (open WMENU))))
  
  ;; implementation@ : implementation^
  (define implementation@
    (compound-unit/sig
      (import)
      (link [SNIP          : snip^         (global-snip@)]
            [MENU          : context-menu^ (widget-context-menu@)]
            [KEYMAP        : keymap^       (widget-keymap@ MENU SNIP)]
            [WIDGET        : widget^       (widget@ KEYMAP)])
      (export (unit SNIP snip)
              (unit WIDGET widget))))
  
  )
