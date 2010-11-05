#lang racket/base
(require unstable/class-iop
         (prefix-in sb: "../syntax-browser/interfaces.rkt"))
(provide (all-defined-out))

(define-interface config<%> (sb:config<%>)
  ((sb:methods:notify draw-arrows?
                      refresh-on-resize?
                      macro-hiding-mode
                      show-hiding-panel?
                      identifier=?
                      highlight-foci?
                      highlight-frontier?
                      show-rename-steps?
                      suppress-warnings?
                      one-by-one?
                      extra-navigation?
                      debug-catch-errors?
                      split-context?)))

(define-interface widget<%> ()
  (get-config
   get-controller
   get-macro-hiding-prefs
   get-step-displayer

   add-trace
   add-deriv

   update/preserve-view
   refresh/resynth

   reset-primary-partition
   remove-current-term
   duplicate-stepper
   show-in-new-frame

   get-preprocess-deriv
   get-show-macro?
))

(define-interface stepper-frame<%> ()
  (get-widget
   get-controller
   add-obsoleted-warning))

(define-interface hiding-prefs<%> ()
  (add-show-identifier
   add-hide-identifier
   set-syntax
   get-policy
   refresh))


(define-interface step-display<%> ()
  (add-syntax
   add-step
   add-error
   add-final
   add-internal-error))


(define-interface term-record<%> ()
  (get-raw-deriv
   get-deriv-hidden?
   get-step-index
   get-step-count
   invalidate-synth!
   invalidate-steps!

   has-prev?
   has-next?
#|
   at-start?
   at-end?
|#
   navigate-to-start
   navigate-to-end
   navigate-previous
   navigate-next
   navigate-to

   on-get-focus
   on-lose-focus

   display-initial-term
   display-final-term
   display-step
   ))

(define-interface director<%> ()
  (add-deriv
   new-stepper))
