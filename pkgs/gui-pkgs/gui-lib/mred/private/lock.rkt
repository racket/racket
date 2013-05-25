#lang racket/base
(require (for-syntax racket/base)
         ffi/unsafe/atomic)

(provide (protect-out as-entry    ;; alias for call-as-atomic
                      as-exit     ;; alias for call-as-nonatomic
                      atomically  ;; assumes no exceptions!
                      entry-point ;; converts a proc body to use as-entry
                      mk-param))  ;; parameter pattern --- out of place here

;; We need atomic mode for a couple of reasons:
;;
;; * We may need to bracket some (trusted) operations so that the
;;   queue thread doesn't poll for events during the operation.
;;   The `atomically' form is ok for that if no exceptions will
;;   be raised. Otherwise, use the more heavyweight `as-entry'.
;;
;; * The racket/gui classes have internal-consistency requirements.
;;   When the user creates an object or calls a method, or when the
;;   system invokes a callback, many steps may be required to
;;   initialize or reset fields to maintain invariants. To ensure that
;;   other threads do not call methods during a time when invariants
;;   do not hold, we force all of the following code to be executed in
;;   a single threaded manner, and we temporarily disable breaks.
;;   The `as-entry' form or `entry-point' wrapper is normally used for
;;   that case.
;;
;; If an exception is raised within an `enter'ed area, control is
;; moved back outside by the exception handler, and then the exception
;; is re-raised. The user can't tell that the exception was caught an
;; re-raised. But without the catch-and-reraise, the user's exception
;; handler might try to use GUI elements from a different thread, or
;; other such things, leading to deadlock.

(define as-entry call-as-atomic)

(define as-exit call-as-nonatomic)

(define-syntax entry-point 
  (lambda (stx)
    (syntax-case stx (lambda #%plain-lambda case-lambda)
      [(_ (lambda args body1 body ...))
       (syntax (lambda args (as-entry (lambda () body1 body ...))))]
      [(_ (#%plain-lambda args body1 body ...))
       (syntax (#%plain-lambda args (as-entry (lambda () body1 body ...))))]
      [(_ (case-lambda [vars body1 body ...] ...))
       (syntax (case-lambda 
                [vars (as-entry (lambda () body1 body ...))]
                ...))])))

(define-syntax-rule (atomically expr ...)
  (begin
    (start-atomic) 
    (begin0 (let () expr ...)
            (end-atomic))))

;; Parameter-method pattern. (Why is this in the "lock" library?)
(define-syntax mk-param
  (lambda (stx)
    (syntax-case stx ()
      [(_ val filter check force-redraw)
       (syntax
        (case-lambda
         [() val]
         [(v) (check v)
          (let ([v2 (filter v)])
            (unless (eq? v2 val)
              (set! val v2)
              (force-redraw)))]))])))
