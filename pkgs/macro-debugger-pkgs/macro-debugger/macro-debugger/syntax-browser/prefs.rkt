#lang racket/base
(require racket/class
         framework/preferences
         macro-debugger/syntax-browser/interfaces
         unstable/gui/notify
         unstable/gui/prefs)
(provide prefs-base%
         syntax-prefs-base%
         syntax-prefs%
         syntax-prefs/readonly%

         pref:invert-colors?)

(preferences:set-default 'SyntaxBrowser:Width 700 number?)
(preferences:set-default 'SyntaxBrowser:Height 600 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelShown #t boolean?)
(preferences:set-default 'SyntaxBrowser:DrawArrows? #t boolean?)

(define pref:width (pref:get/set 'SyntaxBrowser:Width))
(define pref:height (pref:get/set 'SyntaxBrowser:Height))
(define pref:props-percentage (pref:get/set 'SyntaxBrowser:PropertiesPanelPercentage))
(define pref:props-shown? (pref:get/set 'SyntaxBrowser:PropertiesPanelShown))
(define pref:draw-arrows? (pref:get/set 'SyntaxBrowser:DrawArrows?))

(define pref:invert-colors? (pref:get/set 'framework:white-on-black?))

(define prefs-base%
  (class object%
    ;; suffix-option : SuffixOption
    (define-notify suffix-option (new notify-box% (value 'over-limit)))

    ;; pretty-abbrev? : boolean
    (define-notify pretty-abbrev? (new notify-box% (value #t)))

    ;; pretty-styles : ImmutableHash[symbol -> symbol]
    (define-notify pretty-styles
      (new notify-box% (value (make-immutable-hasheq null))))

    ;; syntax-font-size : number/#f
    ;; When non-false, overrides the default font size
    (define-notify syntax-font-size (new notify-box% (value #f)))

    ;; colors : (listof string)
    (define-notify colors
      (new notify-box% (value the-colors)))

    (super-new)))

(define alt-colors
  '("black"
    "red"       "blue"           "forestgreen" "purple"       "brown"
    "firebrick" "darkblue"       "seagreen"    "violetred"    "chocolate"
    "darkred"   "cornflowerblue" "darkgreen"   "indigo"       "sandybrown"
    "orange"    "cadetblue"      "olive"       "mediumpurple" "goldenrod"))

(define the-colors
  '("black" "red" "blue"
    "mediumforestgreen" "darkgreen" 
    "darkred"
    "cornflowerblue" "royalblue" "steelblue" "darkslategray" "darkblue"
    "indigo" "purple" 
    "orange" "salmon" "darkgoldenrod" "olive"))

(define syntax-prefs-base%
  (class* prefs-base% (config<%>)
    (init readonly?)

    (define-syntax-rule (define-pref-notify* (name pref) ...)
      (begin (define-notify name (notify-box/pref pref #:readonly? readonly?)) ...))

    (define-pref-notify*
      (width pref:width)
      (height pref:height)
      (props-percentage pref:props-percentage)
      (props-shown? pref:props-shown?)
      (draw-arrows? pref:draw-arrows?))

    (super-new)))

(define syntax-prefs%
  (class syntax-prefs-base%
    (super-new (readonly? #f))))

(define syntax-prefs/readonly%
  (class syntax-prefs-base%
    (super-new (readonly? #t))))
