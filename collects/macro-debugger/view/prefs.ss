
#lang scheme/base
(require scheme/class
         framework/framework
         "interfaces.ss"
         "../syntax-browser/prefs.ss"
         "../util/notify.ss"
         "../util/misc.ss")
(provide pref:macro-step-limit
         macro-stepper-config-base%
         macro-stepper-config/prefs%
         macro-stepper-config/prefs/readonly%)

(preferences:set-default 'MacroStepper:Frame:Width 700 number?)
(preferences:set-default 'MacroStepper:Frame:Height 600 number?)
(preferences:set-default 'MacroStepper:PropertiesShown? #f boolean?)
(preferences:set-default 'MacroStepper:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'MacroStepper:MacroHidingMode "Standard" string?)
(preferences:set-default 'MacroStepper:ShowSyntaxProperties? #f boolean?)
(preferences:set-default 'MacroStepper:ShowHidingPanel? #t boolean?)
(preferences:set-default 'MacroStepper:IdentifierComparison "bound-identifier=?" string?)
(preferences:set-default 'MacroStepper:HighlightFoci? #t boolean?)
(preferences:set-default 'MacroStepper:HighlightFrontier? #t boolean?)
(preferences:set-default 'MacroStepper:ShowRenameSteps? #f boolean?)
(preferences:set-default 'MacroStepper:SuppressWarnings? #f boolean?)
(preferences:set-default 'MacroStepper:OneByOne? #f boolean?)
(preferences:set-default 'MacroStepper:ExtraNavigation? #f boolean?)
(preferences:set-default 'MacroStepper:DebugCatchErrors? #t boolean?)
(preferences:set-default 'MacroStepper:ForceLetrecTransformation? #f boolean?)
(preferences:set-default 'MacroStepper:SplitContext? #f boolean?)

(preferences:set-default 'MacroStepper:MacroStepLimit 40000
                         (lambda (x) (or (eq? x #f) (exact-positive-integer? x))))

(pref:get/set pref:width MacroStepper:Frame:Width)
(pref:get/set pref:height MacroStepper:Frame:Height)
(pref:get/set pref:props-shown? MacroStepper:PropertiesShown?)
(pref:get/set pref:props-percentage MacroStepper:PropertiesPanelPercentage)
(pref:get/set pref:macro-hiding-mode MacroStepper:MacroHidingMode)
(pref:get/set pref:show-hiding-panel? MacroStepper:ShowHidingPanel?)
(pref:get/set pref:identifier=? MacroStepper:IdentifierComparison)
(pref:get/set pref:highlight-foci? MacroStepper:HighlightFoci?)
(pref:get/set pref:highlight-frontier? MacroStepper:HighlightFrontier?)
(pref:get/set pref:show-rename-steps? MacroStepper:ShowRenameSteps?)
(pref:get/set pref:suppress-warnings? MacroStepper:SuppressWarnings?)
(pref:get/set pref:one-by-one? MacroStepper:OneByOne?)
(pref:get/set pref:extra-navigation? MacroStepper:ExtraNavigation?)
(pref:get/set pref:debug-catch-errors? MacroStepper:DebugCatchErrors?)
(pref:get/set pref:force-letrec-transformation? MacroStepper:ForceLetrecTransformation?)
(pref:get/set pref:split-context? MacroStepper:SplitContext?)

(pref:get/set pref:macro-step-limit MacroStepper:MacroStepLimit)

(define macro-stepper-config-base%
  (class* syntax-prefs-base% (config<%>)
    (notify-methods macro-hiding-mode)
    (notify-methods show-hiding-panel?)
    (notify-methods identifier=?)
    (notify-methods highlight-foci?)
    (notify-methods highlight-frontier?)
    (notify-methods show-rename-steps?)
    (notify-methods suppress-warnings?)
    (notify-methods one-by-one?)
    (notify-methods extra-navigation?)
    (notify-methods debug-catch-errors?)
    (notify-methods force-letrec-transformation?)
    (notify-methods split-context?)
    (super-new)))

(define macro-stepper-config/prefs%
  (class macro-stepper-config-base%
    (connect-to-pref width pref:width)
    (connect-to-pref height pref:height)
    (connect-to-pref props-percentage pref:props-percentage)
    (connect-to-pref props-shown? pref:props-shown?)
    (connect-to-pref macro-hiding-mode pref:macro-hiding-mode)
    (connect-to-pref show-hiding-panel? pref:show-hiding-panel?)
    (connect-to-pref identifier=? pref:identifier=?)
    (connect-to-pref highlight-foci? pref:highlight-foci?)
    (connect-to-pref highlight-frontier? pref:highlight-frontier?)
    (connect-to-pref show-rename-steps? pref:show-rename-steps?)
    (connect-to-pref suppress-warnings? pref:suppress-warnings?)
    (connect-to-pref one-by-one? pref:one-by-one?)
    (connect-to-pref extra-navigation? pref:extra-navigation?)
    (connect-to-pref debug-catch-errors? pref:debug-catch-errors?)
    (connect-to-pref force-letrec-transformation? pref:force-letrec-transformation?)
    (connect-to-pref split-context? pref:split-context?)
    (super-new)))

(define macro-stepper-config/prefs/readonly%
  (class macro-stepper-config-base%
    (connect-to-pref/readonly width pref:width)
    (connect-to-pref/readonly height pref:height)
    (connect-to-pref/readonly macro-hiding-mode pref:macro-hiding-mode)
    (connect-to-pref/readonly props-percentage pref:props-percentage)
    (connect-to-pref/readonly show-hiding-panel? pref:show-hiding-panel?)
    (connect-to-pref/readonly identifier=? pref:identifier=?)
    (connect-to-pref/readonly highlight-foci? pref:highlight-foci?)
    (connect-to-pref/readonly highlight-frontier? pref:highlight-frontier?)
    (connect-to-pref/readonly show-rename-steps? pref:show-rename-steps?)
    (connect-to-pref/readonly suppress-warnings? pref:suppress-warnings?)
    (connect-to-pref/readonly one-by-one? pref:one-by-one?)
    (connect-to-pref/readonly extra-navigation? pref:extra-navigation?)
    (connect-to-pref/readonly debug-catch-errors? pref:debug-catch-errors?)
    (connect-to-pref/readonly force-letrec-transformation? pref:force-letrec-transformation?)
    (connect-to-pref/readonly split-context? pref:split-context?)
    (super-new)))
