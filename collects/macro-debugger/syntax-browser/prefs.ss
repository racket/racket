
(module prefs mzscheme
  (require (lib "class.ss")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "../util/misc.ss")
  (provide syntax-prefs%
           syntax-prefs-mixin

           pref:tabify)

  (preferences:set-default 'SyntaxBrowser:Width 700 number?)
  (preferences:set-default 'SyntaxBrowser:Height 600 number?)
  (preferences:set-default 'SyntaxBrowser:PropertiesPanelPercentage 1/3 number?)
  (preferences:set-default 'SyntaxBrowser:PropertiesPanelShown #t boolean?)

  (pref:get/set pref:width SyntaxBrowser:Width)
  (pref:get/set pref:height SyntaxBrowser:Height)
  (pref:get/set pref:props-percentage SyntaxBrowser:PropertiesPanelPercentage)
  (pref:get/set pref:props-shown? SyntaxBrowser:PropertiesPanelShown)

  (pref:get/set pref:tabify framework:tabify)

  (define syntax-prefs-mixin
    (closure-mixin (syntax-prefs<%>)
      (pref:width pref:width)
      (pref:height pref:height)
      (pref:props-percentage pref:props-percentage)
      (pref:props-shown? pref:props-shown?)))

  (define syntax-prefs% (syntax-prefs-mixin object%))
  )
