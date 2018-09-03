#lang racket/base

;; In addition to its use from "sandman.rkt", this module is meant to
;; be used by a layer above the "thread" implementation, such as the
;; "io" layer.

;; See also "sandman.rkt".

(provide (struct-out sandman))

;; A `sandman` implements several methods, and the sandman implementation
;; gets to pick the representation of <ext-evnt-set> and <handle>, except
;; that #f is the "empty" external event set and #f cannot be a <handle>.
(struct sandman (do-sleep           ; <ext-event-set> -> (void), uses <ext-event-set> plus registered threads
                 do-poll            ; <mode> (thread -> any) -> (void), calls function on any thread to wake up
                 ;                    where <mode> is 'fast or 'slow

                 do-get-wakeup      ; -> <wakeup-handle>, identifies current place
                 do-wakeup          ; <wakeup-handle> -> (void), wakes up `do-sleep` call

                 do-any-sleepers?   ; -> boolean
                 do-sleepers-external-events ; -> <ext-event-set> for sleepers

                 do-add-thread!     ; <thread> <ext-event-set> -> <handle>
                 do-remove-thread!  ; <thread> <handle> -> (void)

                 do-merge-external-event-sets ; <ext-event-set> <ext-event-set> -> <ext-event-set>

                 do-merge-timeout   ; <ext-event-set> <wake-up-date-as-msecs> -> <ext-event-set>
                 do-extract-timeout ; <ext-event-set> -> <wake-up-date-as-msecs>

                 do-condition-wait  ; set a thread to wait on a condition

                 do-condition-poll  ; reschedule awoken threads

                 do-any-waiters?    ; -> boolean

                 lock

                 #;...) ; sandman implementations can add more methods
                 
  #:prefab)
