
(module prefs mzscheme
  (require (lib "unit.ss")
           (lib "framework.ss" "framework")
           "interfaces.ss")
  (provide prefs@)
  
  (define-syntax pref:get/set
    (syntax-rules ()
      [(_ get/set prop)
       (define get/set
         (case-lambda
           [() (preferences:get 'prop)]
           [(newval) (preferences:set 'prop newval)]))]))

  (preferences:set-default 'MacroStepper:Frame:Width 700 number?)
  (preferences:set-default 'MacroStepper:Frame:Height 600 number?)
  (preferences:set-default 'MacroStepper:PropertiesPanelPercentage 1/3 number?)
  (preferences:set-default 'MacroStepper:MacroHiding? #t boolean?)
  (preferences:set-default 'MacroStepper:ShowSyntaxProperties? #f boolean?)
  (preferences:set-default 'MacroStepper:ShowHidingPanel? #t boolean?)
  (preferences:set-default 'MacroStepper:HidePrimitives? #t boolean?)
  (preferences:set-default 'MacroStepper:HideLibs? #t boolean?)
  (preferences:set-default 'MacroStepper:IdentifierComparison "bound-identifier=?" string?)
  (preferences:set-default 'MacroStepper:HighlightFoci? #t boolean?)
  (preferences:set-default 'MacroStepper:HighlightFrontier? #t boolean?)
  (preferences:set-default 'MacroStepper:ShowRenameSteps? #f boolean?)
  (preferences:set-default 'MacroStepper:SuppressWarnings? #f boolean?)
  (preferences:set-default 'MacroStepper:OneByOne? #f boolean?)
  (preferences:set-default 'MacroStepper:ExtraNavigation? #f boolean?)
  (preferences:set-default 'MacroStepper:DebugCatchErrors? #t boolean?)
  (preferences:set-default 'MacroStepper:ForceLetrecTransformation? #f boolean?)

  (define prefs@
    (unit
      (import)
      (export prefs^)
      
      (pref:get/set pref:width MacroStepper:Frame:Width)
      (pref:get/set pref:height MacroStepper:Frame:Height)
      (pref:get/set pref:props-percentage MacroStepper:PropertiesPanelPercentage)
      (pref:get/set pref:macro-hiding? MacroStepper:MacroHiding?)
      (pref:get/set pref:show-syntax-properties? MacroStepper:ShowSyntaxProperties?)
      (pref:get/set pref:show-hiding-panel? MacroStepper:ShowHidingPanel?)
      (pref:get/set pref:hide-primitives? MacroStepper:HidePrimitives?)
      (pref:get/set pref:hide-libs? MacroStepper:HideLibs?)
      (pref:get/set pref:identifier=? MacroStepper:IdentifierComparison)
      (pref:get/set pref:highlight-foci? MacroStepper:HighlightFoci?)
      (pref:get/set pref:highlight-frontier? MacroStepper:HighlightFrontier?)
      (pref:get/set pref:show-rename-steps? MacroStepper:ShowRenameSteps?)
      (pref:get/set pref:suppress-warnings? MacroStepper:SuppressWarnings?)
      (pref:get/set pref:one-by-one? MacroStepper:OneByOne?)
      (pref:get/set pref:extra-navigation? MacroStepper:ExtraNavigation?)
      (pref:get/set pref:debug-catch-errors? MacroStepper:DebugCatchErrors?)
      (pref:get/set pref:force-letrec-transformation? MacroStepper:ForceLetrecTransformation?)
      
      ))
  )
