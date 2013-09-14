#lang racket/base
(require unstable/class-iop)
(provide (all-defined-out))

;; controller
;; Manages the model and view.
;; Propagates status changes from model to view.
(define-interface controller<%> ()
  (get-selected-model
   set-selected-model
   listen-selected-model

   create-model
   on-model-status-change
   register-view
   on-view-shutdown

   ;; field: locked?
   ))

;; result
;; Represents a test (case or suite) together with the state associated
;; with the last run of that test.
(define-interface result<%> ()
  (get-test
   get-parent
   get-name
   get-controller

   finished?
   success?
   failure?
   error?
   has-output?
   has-trash?
   get-timing

   get-total-cases
   get-total-successes
   get-total-failures))

(define-interface case<%> (result<%>)
  (update
   get-result
   get-output
   get-trash
   get-property
   get-property-set
   get-all-properties))

(define-interface suite<%> (result<%>)
  (get-children
   add-child
   finish!
   on-child-status-change))


;; view
;; Presents a graphical interface for inspecting and running tests.
(define-interface view<%> ()
  (create-view-link
   queue-for-update
   shutdown))

;; style-map
;; Maps symbolic style names ('bold, 'red) to GRacket styles.
(define-interface style-map<%> ()
  (get-style))
