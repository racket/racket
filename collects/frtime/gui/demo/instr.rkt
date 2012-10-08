#lang frtime
(require frtime/gui/mixin-macros frtime/gui/aux-mixin-macros)
(require racket/class) ; require class utilities
(require mred) ; require base mred library


;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior->callbacks ;; (aux-mixin-macros.rkt)
;;;;;;;;;;;;;;;;;;;;;;;;;

; create a mixin using the macro
(define label-lifter
  (behavior->callbacks label ; name of the init field
                       set-label ; callback to invoke
                       ))

; apply the mixin to a class (and a default value).
; whenever the "label" argument of a fr-label-frame changes,
; set-label will be invoked with the current value of the
; supplied label behavior
(define fr-label-frame% (label-lifter "" frame%))

; each second, the label will be changed to the current value of
; seconds
(define my-frame
  (new fr-label-frame% (label (number->string seconds))))


;;;;;;;;;;;;;;;;;;;;;;;
;; events->callbacks ;; (mixin-macros.rkt)
;;;;;;;;;;;;;;;;;;;;;;;

; create a mixin using the macro
(define set-value-lifter
  (events->callbacks value-set-e ; name of the init field
                     set-value ; callback to invoke
                     ))

; apply the mixin
; fr-value-text-field%s will set their value to the value of 
; the event occurrences supplied in the initialization argument
; value-set-e
(define fr-value-text-field% (set-value-lifter text-field%))

; every 10 seconds, the my-text will set its value to be that
; of seconds
(define my-text
  (new fr-value-text-field% 
       (label "")
       (value-set-e 
        (map-e number->string
               (filter-e (lambda (evt) (zero? (modulo evt 10)))
                         (changes seconds))))
       (parent my-frame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; callbacks->args-evts ;; (mixin-macros.rkt)
;;;;;;;;;;;;;;;;;;;;;;;;;;

; create a mixin using the macro
(define focus-lifter
  (callbacks->args-evts focus-events ; name of event stream
                        on-focus ; callback being watched
                        ))

; apply the mixin
; whenever on-focus is called in a fr-focus-check-box%,
; a list of the argument (is-focused?) is sent to an
; event stream
(define fr-focus-check-box% (focus-lifter (lambda (x) x) check-box%))

; the focus-events-event-processor is applied to the
; event stream that carries events from on-focus.
(define my-cb1 (new fr-focus-check-box%
                    (parent my-frame)
                    (label "Check1")))

; Because these events are lists of one element (the only
; argument to on-focus), car effectively extracts the
; value indicated by the callback (is-focused?).
(define my-cb2 (new fr-focus-check-box%
                    (parent my-frame)
                    (focus-events-event-processor
                     (lambda (es) (map-e car es)))
                    (label "Check2")))

; focus-events-event-processor is a function of one
; argument, an event stream, and can return anything
; that is convenient
(define my-cb3 (new fr-focus-check-box%
                    (parent my-frame)
                    (focus-events-event-processor
                     (lambda (es) (hold (map-e car es) #f)))
                    (label "Check3")))

; get the streams from the check boxes
(printf "callbacks->args-evts:\n")
(send my-cb1 get-focus-events)
(send my-cb2 get-focus-events)
(send my-cb3 get-focus-events)


;;;;;;;;;;;;;;;;;;;
;; mixin-merge-e ;; (aux-mixin-macros.rkt)
;;;;;;;;;;;;;;;;;;;


; see callbacks->args-evts
(define key-events-lifter
  (callbacks->args-evts key-events on-subwindow-char))

; you can stack these mixins as long as you are
; careful about reuse of names
(define fr-focus-and-key-events-check-box%
  (key-events-lifter
   (lambda (evt-src) (map-e cdr evt-src))
   fr-focus-check-box%))

; apply the macro to generate a mixin
(define merge-lifter
  (mixin-merge-e
   all-events ; name of the event stream created
   get-key-events ; getter for one event stream
   get-focus-events ; getter for second event stream
   ))

; apply the mixin
(define fr-merged-access-check-box%
  (merge-lifter fr-focus-and-key-events-check-box%))

; now, there is an additional getter method.
; the two event streams for key-events and
; focus-events are still around, but now
; there is also all-events
(define my-cb4 (new fr-merged-access-check-box%
                    (parent my-frame)
                    (key-events-event-processor
                     (lambda (es)
                       (map-e (lambda (evt) (send (cadr evt) get-key-code)) es)))
                    (focus-events-event-processor
                     (lambda (es)
                       (map-e car es)))
                    (label "Check4")))

(printf "mixin-merge-e:\n")

(send my-cb4 get-focus-events) ; focus-events
(send my-cb4 get-key-events) ; key-events
(send my-cb4 get-all-events) ; (merge-e focus-events key-events)
                          

;; SHOW THE FRAME
(send my-frame show #t)
