
(module view mzscheme
  (require (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "gui.ss")
  (provide (all-defined))

  (define view-base@
    (unit
      (import)
      (export view-base^)

      (define base-frame%
        (frame:standard-menus-mixin (frame:basic-mixin frame%)))))

  (define-values/invoke-unit
    (compound-unit
      (import)
      (link [((BASE : view-base^)) view-base@]
            [((STEPPER : view^)) pre-stepper@ BASE])
      (export STEPPER))
    (import)
    (export view^))
  )
