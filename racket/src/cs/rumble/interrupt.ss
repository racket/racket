
;; Like `with-interrupts-disabled`, but with no winders
;; and always returning a single value. Avoiding winders
;; is important in "foreign.ss" so that callbacks do
;; not return to a world with Scheme-level winders, which
;; will not interact correctly with engines.
(define-syntax-rule (with-interrupts-disabled* e0 e ...)
  (begin
    (disable-interrupts)
    (let ([v (begin e0 e ...)])
      (enable-interrupts)
      v)))

;; Enabling uninterrupted mode defers a timer callback
;; until leaving uninterrupted mode. This is the same
;; as disabling and enabling interrupts at the Chez
;; level, but cheaper and more limited. Uninterrupted
;; mode should be used only by the implementation of
;; engines and control in "engine.ss" and "control.ss".

(define-virtual-register current-in-engine-uninterrupted #f)
(define-virtual-register pending-interrupt-callback #f)

(meta-cond
 [(= (optimize-level) 3)
  (define-syntax CHECK-engine-uninterrupted
    (syntax-rules ()
      [(_ e ...) (void)]))]
 [else
  (define-syntax CHECK-engine-uninterrupted
    (syntax-rules ()
      [(_ e ...) (begin e ...)]))])

(define (start-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (when (current-in-engine-uninterrupted)
     (internal-error 'start-engine-uninterrupted (format "~a: already started" who))))
  (current-in-engine-uninterrupted #t))

(define (end-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (unless (current-in-engine-uninterrupted)
     (internal-error 'end-engine-uninterrupted (format "~a: not started" who))))
  (current-in-engine-uninterrupted #f)
  (when (pending-interrupt-callback)
    (pariah
     (let ([callback (pending-interrupt-callback)])
       (pending-interrupt-callback #f)
       (callback)))))

(define (assert-in-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (unless (current-in-engine-uninterrupted)
     (internal-error 'assert-in-engine-uninterrupted (format "~a: assertion failed" who)))))

(define (assert-not-in-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (when (current-in-engine-uninterrupted)
     (internal-error 'assert-not-in-engine-uninterrupted (format "~a: assertion failed" who)))))

;; An implicit context is when a relevant interrupt can't happen, but
;; `assert-in-engine-uninterrupted` might be called.

(define (start-implicit-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (when (current-in-engine-uninterrupted)
     (internal-error 'start-implicit-engine-uninterrupted (format "~a: already started" who)))
   (unless (fx= 0 (set-timer 0))
     (internal-error 'start-implicit-engine-uninterrupted (format "~a: timer is running" who)))
   (current-in-engine-uninterrupted #t)))

(define (end-implicit-engine-uninterrupted who)
  (CHECK-engine-uninterrupted
   (unless (current-in-engine-uninterrupted)
     (internal-error 'end-implicit-engine-uninterrupted (format "~a: not started" who)))
   (current-in-engine-uninterrupted #f)))

(define (internal-error who s)
  (CHECK-engine-uninterrupted
   (chez:fprintf (current-error-port) "~a: ~a\n" who s)
   (#%exit 1)))
