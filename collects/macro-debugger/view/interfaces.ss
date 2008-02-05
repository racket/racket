
#lang scheme/base
(require scheme/unit)
(provide (all-defined-out))

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
   pref:macro-hiding-mode
   pref:show-syntax-properties?
   pref:show-hiding-panel?
   pref:identifier=?
   pref:show-rename-steps?
   pref:highlight-foci?
   pref:highlight-frontier?
   pref:suppress-warnings?
   pref:one-by-one?
   pref:extra-navigation?
   pref:debug-catch-errors?
   pref:force-letrec-transformation?
   ))

;; macro-stepper-config%
;;   all fields are notify-box% objects
;;     width
;;     height
;;     macro-hiding?
;;     hide-primitives?
;;     hide-libs?
;;     show-syntax-properties?
;;     show-hiding-panel?
;;     show-rename-steps?
;;     highlight-foci?
