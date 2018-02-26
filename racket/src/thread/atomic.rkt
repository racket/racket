#lang racket/base
(require "engine.rkt"
         "internal-error.rkt"
         "debug.rkt")

(provide atomically
         current-atomic

         start-atomic
         end-atomic

         start-atomic/no-interrupts
         end-atomic/no-interrupts

         set-end-atomic-callback!

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

(define (start-atomic)
  (current-atomic (add1 (current-atomic))))

(define (end-atomic)
  (define n (sub1 (current-atomic)))
  (cond
    [(and end-atomic-callback
          (zero? n))
     (define cb end-atomic-callback)
     (set! end-atomic-callback #f)
     (current-atomic n)
     (cb)]
    [else
     (current-atomic n)]))

(define (start-atomic/no-interrupts)
  (start-atomic)
  (host:disable-interrupts))

(define (end-atomic/no-interrupts)
  (host:enable-interrupts)
  (end-atomic))

;; ----------------------------------------

(define end-atomic-callback #f)

(define (set-end-atomic-callback! cb)
  (set! end-atomic-callback cb))


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
