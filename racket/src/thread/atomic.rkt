#lang racket/base
(require racket/fixnum
         "host.rkt"
         "place-local.rkt"
         "internal-error.rkt"
         "debug.rkt")

(provide atomically
         current-atomic

         start-atomic
         end-atomic

         atomically/no-interrupts
         start-atomic/no-interrupts
         end-atomic/no-interrupts

         in-atomic-mode?

         add-end-atomic-callback!

         start-implicit-atomic-mode
         end-implicit-atomic-mode
         assert-atomic-mode)

;; This definition is specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register:
(define current-atomic (make-pthread-parameter 0))

(define-syntax-rule (atomically expr ...)
  (begin
    (start-atomic)
    (begin0
     (let () expr ...)
     (end-atomic))))

(define-syntax-rule (atomically/no-interrupts expr ...)
  (begin
    (start-atomic/no-interrupts)
    (begin0
     (let () expr ...)
     (end-atomic/no-interrupts))))

(define (start-atomic)
  (current-atomic (fx+ (current-atomic) 1)))

(define (end-atomic)
  (define n (fx- (current-atomic) 1))
  (cond
    [(fx= n 0)
     (if (eq? 0 (end-atomic-callback))
         (current-atomic n)
         (do-end-atomic-callback))]
    [(fx< n 0) (bad-end-atomic)]
    [else
     ;; There's a small chance that `end-atomic-callback`
     ;; was set by the scheduler after the check and
     ;; before we exit atomic mode. Make sure that rare
     ;; possibility remains ok.
     (current-atomic n)]))

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

(define (start-atomic/no-interrupts)
  (start-atomic)
  (host:disable-interrupts))

(define (end-atomic/no-interrupts)
  (host:enable-interrupts)
  (end-atomic))

(define (in-atomic-mode?)
  (positive? (current-atomic)))

;; ----------------------------------------

;; A "list" of callbacks to run when exiting atomic mode,
;; but the list is terminated by 0 insteda of '().
;; This definition is converted to a virtual register on
;; Chez Scheme, which explains why 0 is the "none" value.
(define end-atomic-callback (make-pthread-parameter 0))

;; in atomic mode, but need to disable interrupts to ensure
;; no race with the scheduler
(define (add-end-atomic-callback! cb)
  (host:disable-interrupts)
  (end-atomic-callback (cons cb (end-atomic-callback)))
  (host:enable-interrupts))

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

  (define-syntax-rule (assert-atomic-mode)
    (unless (or (current-implicit-atomic)
                (positive? (current-atomic)))
      (internal-error "should be in atomic mode")))]
 #:off
 [(define-syntax-rule (start-implicit-atomic-mode) (begin))
  (define-syntax-rule (end-implicit-atomic-mode) (begin))
  (define-syntax-rule (assert-atomic-mode) (begin))])
