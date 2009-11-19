#lang scheme/base
(require scheme/class
         framework/framework
         "interfaces.ss"
         "../syntax-browser/prefs.ss"
         unstable/gui/notify
         unstable/gui/prefs)
(provide pref:macro-step-limit
         macro-stepper-config-base%
         macro-stepper-config/prefs%
         macro-stepper-config/prefs/readonly%)

(preferences:set-default 'MacroStepper:Frame:Width 700 number?)
(preferences:set-default 'MacroStepper:Frame:Height 600 number?)
(preferences:set-default 'MacroStepper:PropertiesShown? #f boolean?)
(preferences:set-default 'MacroStepper:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'MacroStepper:MacroHidingMode "Standard" string?)
(preferences:set-default 'MacroStepper:ShowHidingPanel? #t boolean?)
(preferences:set-default 'MacroStepper:IdentifierComparison "bound-identifier=?" string?)
(preferences:set-default 'MacroStepper:HighlightFoci? #t boolean?)
(preferences:set-default 'MacroStepper:HighlightFrontier? #t boolean?)
(preferences:set-default 'MacroStepper:ShowRenameSteps? #f boolean?)
(preferences:set-default 'MacroStepper:SuppressWarnings? #f boolean?)
(preferences:set-default 'MacroStepper:OneByOne? #f boolean?)
(preferences:set-default 'MacroStepper:ExtraNavigation? #f boolean?)
(preferences:set-default 'MacroStepper:DebugCatchErrors? #t boolean?)
(preferences:set-default 'MacroStepper:SplitContext? #f boolean?)
(preferences:set-default 'MacroStepper:MacroStepLimit 40000
                         (lambda (x) (or (eq? x #f) (exact-positive-integer? x))))

(define pref:width (pref:get/set 'MacroStepper:Frame:Width))
(define pref:height (pref:get/set 'MacroStepper:Frame:Height))
(define pref:props-shown? (pref:get/set 'MacroStepper:PropertiesShown?))
(define pref:props-percentage (pref:get/set 'MacroStepper:PropertiesPanelPercentage))
(define pref:macro-hiding-mode (pref:get/set 'MacroStepper:MacroHidingMode))
(define pref:show-hiding-panel? (pref:get/set 'MacroStepper:ShowHidingPanel?))
(define pref:identifier=? (pref:get/set 'MacroStepper:IdentifierComparison))
(define pref:highlight-foci? (pref:get/set 'MacroStepper:HighlightFoci?))
(define pref:highlight-frontier? (pref:get/set 'MacroStepper:HighlightFrontier?))
(define pref:show-rename-steps? (pref:get/set 'MacroStepper:ShowRenameSteps?))
(define pref:suppress-warnings? (pref:get/set 'MacroStepper:SuppressWarnings?))
(define pref:one-by-one? (pref:get/set 'MacroStepper:OneByOne?))
(define pref:extra-navigation? (pref:get/set 'MacroStepper:ExtraNavigation?))
(define pref:debug-catch-errors? (pref:get/set 'MacroStepper:DebugCatchErrors?))
(define pref:split-context? (pref:get/set 'MacroStepper:SplitContext?))
(define pref:macro-step-limit (pref:get/set 'MacroStepper:MacroStepLimit))


(define macro-stepper-config-base%
  (class* prefs-base% (config<%>)
    (init-field readonly?)

    (define-syntax-rule (define-pref-notify* (name pref) ...)
      (begin (define-notify name (notify-box/pref pref #:readonly? readonly?)) ...))

    (define-pref-notify*
      (width pref:width)
      (height pref:height)
      (props-percentage pref:props-percentage)
      (props-shown? pref:props-shown?)
      (macro-hiding-mode pref:macro-hiding-mode)
      (show-hiding-panel? pref:show-hiding-panel?)
      (identifier=? pref:identifier=?)
      (highlight-foci? pref:highlight-foci?)
      (highlight-frontier? pref:highlight-frontier?)
      (show-rename-steps? pref:show-rename-steps?)
      (suppress-warnings? pref:suppress-warnings?)
      (one-by-one? pref:one-by-one?)
      (extra-navigation? pref:extra-navigation?)
      (debug-catch-errors? pref:debug-catch-errors?)
      (split-context? pref:split-context?))
    (super-new)))

(define macro-stepper-config/prefs%
  (class macro-stepper-config-base%
    (super-new (readonly? #f))))

(define macro-stepper-config/prefs/readonly%
  (class macro-stepper-config-base%
    (super-new (readonly? #t))))
