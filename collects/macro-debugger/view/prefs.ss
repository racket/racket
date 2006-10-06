
(module prefs mzscheme
  (require (lib "unitsig.ss")
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

  (define prefs@
    (unit/sig prefs^
      (import)
      
      (preferences:set-default 'MacroStepper:Frame:Width 700 number?)
      (preferences:set-default 'MacroStepper:Frame:Height 600 number?)
      (preferences:set-default 'MacroStepper:PropertiesPanelPercentage 1/3 number?)
      (preferences:set-default 'MacroStepper:MacroHiding? #t boolean?)
      (preferences:set-default 'MacroStepper:ShowHidingPanel? #t boolean?)
      (preferences:set-default 'MacroStepper:HidePrimitives? #t boolean?)
      (preferences:set-default 'MacroStepper:HideLibs? #t boolean?)
      (preferences:set-default 'MacroStepper:IdentifierComparison "bound-identifier=?" string?)
      
      (pref:get/set pref:width MacroStepper:Frame:Width)
      (pref:get/set pref:height MacroStepper:Frame:Height)
      (pref:get/set pref:props-percentage MacroStepper:PropertiesPanelPercentage)
      (pref:get/set pref:macro-hiding? MacroStepper:MacroHiding?)
      (pref:get/set pref:show-hiding-panel? MacroStepper:ShowHidingPanel?)
      (pref:get/set pref:hide-primitives? MacroStepper:HidePrimitives?)
      (pref:get/set pref:hide-libs? MacroStepper:HideLibs?)
      (pref:get/set pref:identifier=? MacroStepper:IdentifierComparison)
      ))
  )
