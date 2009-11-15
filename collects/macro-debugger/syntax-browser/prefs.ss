
#lang scheme/base
(require scheme/class
         framework/framework
         "interfaces.ss"
         unstable/gui/notify
         unstable/gui/prefs)
(provide prefs-base%
         syntax-prefs-base%
         syntax-prefs%
         syntax-prefs/readonly%)

(preferences:set-default 'SyntaxBrowser:Width 700 number?)
(preferences:set-default 'SyntaxBrowser:Height 600 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelShown #t boolean?)

(pref:get/set pref:width SyntaxBrowser:Width)
(pref:get/set pref:height SyntaxBrowser:Height)
(pref:get/set pref:props-percentage SyntaxBrowser:PropertiesPanelPercentage)
(pref:get/set pref:props-shown? SyntaxBrowser:PropertiesPanelShown)

(define prefs-base%
  (class object%
    ;; suffix-option : SuffixOption
    (field/notify suffix-option (new notify-box% (value 'over-limit)))

    ;; syntax-font-size : number/#f
    ;; When non-false, overrides the default font size
    (field/notify syntax-font-size (new notify-box% (value #f)))

    ;; colors : (listof string)
    (field/notify colors 
      (new notify-box%
           (value '("black" "red" "blue"
                    "mediumforestgreen" "darkgreen" 
                    "darkred"
                    "cornflowerblue" "royalblue" "steelblue" "darkslategray" "darkblue"
                    "indigo" "purple" 
                    "orange" "salmon" "darkgoldenrod" "olive"))))

    (super-new)))

(define syntax-prefs-base%
  (class* prefs-base% (config<%>)
    ;; width, height : number
    (notify-methods width)
    (notify-methods height)

    ;; props-percentage : ...
    (notify-methods props-percentage)

    ;; props-shown? : boolean
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
