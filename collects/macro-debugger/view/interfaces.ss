
(module interfaces mzscheme
  (require (lib "unitsig.ss"))
  (provide (all-defined))
  
  ;; Signatures

  (define-signature view^
    (macro-stepper-frame%
     macro-stepper-widget%
     make-macro-stepper
     go
     go/deriv))

  (define-signature view-base^
    (base-frame%))

  (define-signature prefs^
    (pref:width
     pref:height
     pref:props-percentage
     pref:macro-hiding?
     pref:show-hiding-panel?
     pref:hide-primitives?
     pref:hide-libs?
     pref:identifier=?))

  )
