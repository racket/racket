#lang racket/base
(require racket/fixnum
         "host.rkt"
         "place-local.rkt"
         "internal-error.rkt"
         "parameter.rkt"
         "debug.rkt"
         (for-syntax racket/base))

(provide current-atomic

         start-uninterruptible
         end-uninterruptible

         start-atomic
         end-atomic
         abort-atomic

         end-atomic/no-barrier-exit

         atomically
         atomically/no-gc-interrupts

         start-uninterruptible/no-gc-interrupts
         end-uninterruptible/no-gc-interrupts
         start-atomic/no-gc-interrupts
         end-atomic/no-gc-interrupts

         in-atomic-mode?
         not-atomic-mode?

         future-barrier
         future-barrier-exit
         atomically/no-barrier-exit

         add-end-atomic-callback!
         flush-end-atomic-callbacks!

         start-implicit-atomic-mode
         end-implicit-atomic-mode
         assert-atomic-mode

         assert-no-end-atomic-callbacks

         assert

         set-future-block!)

;; allowed in a future pthread: does not necessary make a Racket (or
;; Scheme) thread atomic with respect to other threads, but does
;; ensure that a thread or future will be allowed to continue to
;; `end-uninterruptible` before it can be externally stopped
(define (start-uninterruptible)
  (current-atomic (fx+ (current-atomic) 1)))
(define (end-uninterruptible)
  (end-atomic/no-barrier-exit))

;; "atomically" is atomic within a place; when a future-running
;; pthread tries to enter atomic mode, it is suspended
(define-syntax atomically
  (syntax-rules (void)
    [(_ expr ... (void)) ; `(void)` => no need for `begin0`
     (begin
       (start-atomic)
       expr ...
       (end-atomic))]
    [(_ expr ...)
     (begin
       (start-atomic)
       (begin0
         (let () expr ...)
         (end-atomic)))]))

(define-syntax-rule (atomically/no-gc-interrupts expr ...)
  (begin
    (start-atomic/no-gc-interrupts)
    (begin0
     (let () expr ...)
     (end-atomic/no-gc-interrupts))))

(define-syntax-rule (atomically/no-barrier-exit expr ...)
  (begin
    (start-atomic)
    (begin0
     (let () expr ...)
     (end-atomic/no-barrier-exit))))

;; inlined in Chez Scheme embedding:
(define (start-atomic)
  ;; Although it's adjusting atomicity for the thread scheduler,
  ;; this function is documented as working in any Scheme thread.
  ;; The current implementation relies on parameters like
  ;; `future-barrier` and `current-atomic` being virtual registers.
  (future-barrier)
  (current-atomic (fx+ (current-atomic) 1)))

;; inlined in Chez Scheme embedding:
(define (end-atomic)
  ;; See `start-atomic` note on calls from any Scheme thread
  (define n (fx- (current-atomic) 1))
  (cond
    [(fx= n 0)
     ;; There's a small chance that `end-atomic-callback`
     ;; was set by the scheduler after the check and
     ;; before we exit atomic mode. Make sure that rare
     ;; possibility remains ok. There are also places that
     ;; exit atomic mode by decrementing `(current-atomic)`
     ;; directly; those are places where an arbitrary callback
     ;; is not allowed, such as in a foreign callbacks, and in
     ;; that case, we end up delaying the callback until a
     ;; timer interrupt.
     (if (eq? 0 (end-atomic-callback))
         (current-atomic n)
         (do-end-atomic-callback))
     (future-barrier-exit)]
    [(fx< n 0) (bad-end-atomic)]
    [else
     (current-atomic n)]))

;; like `end-atomic`, but for use by anything
;; potentially on the path to an block-handling operation
;; in a Racket thread for a parallel thread, but where an atomic
;; region was created for the handling thread's own purposes and
;; not to act as an atomic region for the parallel thread
(define (end-atomic/no-barrier-exit)
  (define n (fx- (current-atomic) 1))
  (cond
    [(fx= n 0)
     (if (eq? 0 (end-atomic-callback))
         (current-atomic n)
         (do-end-atomic-callback))]
    [(fx< n 0) (bad-end-atomic)]
    [else (current-atomic n)]))

;; intended to avoid an infinite loop of "can't do that in atomic
;; mode" exceptions when things have gone terribly wrong, assume that
;; we're in a state where anything can happen, anyway
(define (abort-atomic)
  (current-atomic 0)
  (end-atomic-callback 0))

(define (do-end-atomic-callback)
  (define cbs (end-atomic-callback))
  (end-atomic-callback 0)
  (current-atomic 0)
  (let loop ([cbs cbs])
    (unless (eq? cbs 0)
      ((car cbs))
      (loop (cdr cbs)))))

(define (bad-end-atomic)
  (internal-error "not in atomic mode to end"))

(define (start-atomic/no-gc-interrupts)
  ;; start atomide mode *before* disabling interrupts, since we
  ;; want any needed transition to a Racket thread to happen first
  (start-atomic)
  (host:disable-interrupts))

(define (end-atomic/no-gc-interrupts)
  (host:enable-interrupts)
  (end-atomic))

(define (start-uninterruptible/no-gc-interrupts)
  (start-uninterruptible)
  (host:disable-interrupts))

(define (end-uninterruptible/no-gc-interrupts)
  (host:enable-interrupts)
  (end-uninterruptible))

(define (in-atomic-mode?)
  (not (eqv? (current-atomic) 0)))
(define (not-atomic-mode?)
  (eqv? (current-atomic) 0))

;; inlined in Chez Scheme embedding;
;; calling `future-barrier` kicks a future computation that is running
;; in a futher pthread over to a Racket thread, either by blocking a
;; future to moving the continuation of a parallel thread (which is
;; implemented in part by a future) over to its Racket thread half;
;; a `future-barrier-exit` call can move the continuation back to
;; a future pthread; if a `future-barrier` call does not have a
;; `future-barrier-exit` later, then a continuation may stay in a
;; Racket thread, which should be ok, and it can get migrated by
;; some later `future-barrier-exit` (after another `future-barrier`);
;; there's also the special case of `end-atomic/no-barrier-exit`,
;; which avoids `future-barrier-exit` because atomic mode was not
;; entered on behalf of a future
(define (future-barrier)
  (when (current-future)
    ;; performs its own check for `(current-future)`, so the preceeding
    ;; check is just an inlined shortcut
    (future-block-for-atomic)))
(define (future-barrier-exit)
  (when (current-future)
    ;; performs its own check for `(current-future)`...
    (future-unblock-for-atomic)))

;; ----------------------------------------

;; A "list" of callbacks to run when exiting atomic mode,
;; but the list is terminated by 0 insteda of '().
;; This definition is converted to a virtual register on
;; Chez Scheme, which explains why 0 is the "none" value.
(define end-atomic-callback (make-pthread-parameter 0))

;; If we're in a situation like an atomic foreign callback, which
;; exits atomic mode by decrementing `current-atomic` directly, then a
;; registered callback might get flushed and never run if the thread
;; ends or gets swapped out. So, only use this for things where it's
;; ok to drop the callback on those unusual boundaries.
(define (add-end-atomic-callback! cb)
  ;; in atomic mode, but need to disable interrupts to ensure
  ;; no race with the scheduler
  (host:disable-interrupts)
  (define all-cbs (end-atomic-callback))
  (let loop ([cbs all-cbs])
    (cond
      [(eq? cbs 0)
       (end-atomic-callback (cons cb all-cbs))]
      [else
       (unless (eq? (car cbs) cb)
         (loop (cdr cbs)))]))
  (host:enable-interrupts))

(define (flush-end-atomic-callbacks!)
  (end-atomic-callback 0))

;; ----------------------------------------

(define future-block-for-atomic (lambda () (void)))
(define future-unblock-for-atomic (lambda () (void)))

(define (set-future-block! block unblock)
  (set! future-block-for-atomic block)
  (set! future-unblock-for-atomic unblock))

;; ----------------------------------------

(debug-select
 #:on
 [(define current-implicit-atomic (make-pthread-parameter #t))

  (define (start-implicit-atomic-mode)
    (when (current-implicit-atomic)
      (internal-error "already implicitly in atomic mode?"))
    (current-implicit-atomic #t))

  (define (end-implicit-atomic-mode)
    (unless (current-implicit-atomic)
      (internal-error "not implicitly in atomic mode?"))
    (current-implicit-atomic #f))

  (define-syntax (assert-atomic-mode stx)
    (syntax-case stx ()
      [(_)
       #`(unless (or (current-implicit-atomic)
                     (positive? (current-atomic)))
           (internal-error #,(format "should be in atomic mode: ~s" stx)))]))

  (define (assert-no-end-atomic-callbacks)
    (unless (eq? 0 (end-atomic-callback))
      (internal-error "non-empty end-atomic callbacks")))
  (define-syntax (assert stx)
    (syntax-case stx ()
      [(_ e)
       #`(unless e
           (internal-error #,(format "assertion failed: ~s" (syntax->datum #'e))))]))]
 #:off
 [(define-syntax-rule (start-implicit-atomic-mode) (begin))
  (define-syntax-rule (end-implicit-atomic-mode) (begin))
  (define-syntax-rule (assert-atomic-mode) (begin))
  (define-syntax-rule (assert-no-end-atomic-callbacks) (begin))
  (define-syntax-rule (assert e) (begin))])
