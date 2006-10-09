
(module view mzscheme
  (require (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "gui.ss")
  (provide (all-defined))

  (define view-base@
    (unit/sig view-base^
      (import)
      (define base-frame%
        (frame:standard-menus-mixin (frame:basic-mixin frame%)))))

  (define-values/invoke-unit/sig view^
    (compound-unit/sig
      (import)
      (link [BASE : view-base^ (view-base@)]
            [STEPPER : view^ (pre-stepper@ BASE)])
      (export (open STEPPER))))
  )
