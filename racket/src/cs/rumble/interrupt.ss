
;; Enabling uninterrupted mode defers a timer callback
;; until leaving uninterrupted mode. This is the same
;; as disabling and enabling interrupts at the Chez
;; level, but cheaper and more limited. Uninterrupted
;; mode should be used only by the implementation of
;; engines and control in "engine.ss" and "control.ss".

(define-virtual-register current-in-uninterrupted #f)
(define-virtual-register pending-interrupt-callback #f)

(define-syntax CHECK-uninterrupted
  (syntax-rules ()
    [(_ e ...) (void) #;(begin e ...)]))

(define (start-uninterrupted who)
  (CHECK-uninterrupted
   (when (current-in-uninterrupted)
     (internal-error 'start-uninterrupted (format "~a: already started" who))))
  (current-in-uninterrupted #t))

(define (end-uninterrupted who)
  (CHECK-uninterrupted
   (unless (current-in-uninterrupted)
     (internal-error 'end-uninterrupted (format "~a: not started" who))))
  (current-in-uninterrupted #f)
  (when (pending-interrupt-callback)
    (pariah
     (let ([callback (pending-interrupt-callback)])
       (pending-interrupt-callback #f)
       (callback)))))

(define (assert-in-uninterrupted)
  (CHECK-uninterrupted
   (unless (current-in-uninterrupted)
     (internal-error 'assert-in-uninterrupted "assertion failed"))))

(define (assert-not-in-uninterrupted)
  (CHECK-uninterrupted
   (when (current-in-uninterrupted)
     (internal-error 'assert-not-in-uninterrupted "assertion failed"))))

;; An implicit context is when a relevant interrupt can't happen, but
;; `assert-in-uninterrupted` might be called.

(define (start-implicit-uninterrupted who)
  (CHECK-uninterrupted
   (when (current-in-uninterrupted)
     (internal-error 'start-implicit-uninterrupted "already started"))
   (current-in-uninterrupted #t)))

(define (end-implicit-uninterrupted who)
  (CHECK-uninterrupted
   (unless (current-in-uninterrupted)
     (internal-error 'end-implicit-uninterrupted "not started"))
   (current-in-uninterrupted #f)))

(define (internal-error who s)
  (CHECK-uninterrupted
   (chez:fprintf (current-error-port) "~a: ~a\n" who s)
   (#%exit 1)))
