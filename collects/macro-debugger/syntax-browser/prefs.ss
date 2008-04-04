
#lang scheme/base
(require scheme/class
         framework/framework
         "interfaces.ss"
         "../util/notify.ss"
         "../util/misc.ss")
(provide syntax-prefs%
         syntax-prefs/readonly%

         #;pref:tabify
         #;pref:height
         #;pref:width
         #;pref:props-percentage)

(preferences:set-default 'SyntaxBrowser:Width 700 number?)
(preferences:set-default 'SyntaxBrowser:Height 600 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelShown #t boolean?)

(pref:get/set pref:width SyntaxBrowser:Width)
(pref:get/set pref:height SyntaxBrowser:Height)
(pref:get/set pref:props-percentage SyntaxBrowser:PropertiesPanelPercentage)
(pref:get/set pref:props-shown? SyntaxBrowser:PropertiesPanelShown)
(pref:get/set pref:tabify framework:tabify)

(define syntax-prefs-base%
  (class object%
    (notify-methods width)
    (notify-methods height)
    (notify-methods props-percentage)
    (notify-methods props-shown?)
    (super-new)))

(define syntax-prefs%
  (class syntax-prefs-base%
    (connect-to-pref width pref:width)
    (connect-to-pref height pref:height)
    (connect-to-pref props-percentage pref:props-percentage)
    (connect-to-pref props-shown? pref:props-shown?)
    (super-new)))

(define syntax-prefs/readonly%
  (class syntax-prefs-base%
    (connect-to-pref/readonly width pref:width)
    (connect-to-pref/readonly height pref:height)
    (connect-to-pref/readonly props-percentage pref:props-percentage)
    (connect-to-pref/readonly props-shown? pref:props-shown?)
    (super-new)))
