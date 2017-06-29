#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic
         (only-in '#%unsafe
                  unsafe-make-custodian-at-root
                  unsafe-custodian-register
                  unsafe-custodian-unregister))

(provide (protect-out make-custodian-at-root
                      register-custodian-shutdown
                      register-finalizer-and-custodian-shutdown
                      unregister-custodian-shutdown))

(define (make-custodian-at-root)
  (unsafe-make-custodian-at-root))

(define (register-custodian-shutdown obj proc [custodian (current-custodian)]
                                     #:at-exit? [at-exit? #f]
                                     #:weak? [weak? #f])
  (unsafe-custodian-register custodian obj proc at-exit? weak?))

(define (unregister-custodian-shutdown obj mref)
  (when mref
    (unsafe-custodian-unregister obj mref)))

(define (register-finalizer-and-custodian-shutdown value callback
                                                   [custodian (current-custodian)]
                                                   #:at-exit? [at-exit? #f]
                                                   #:custodian-unavailable [custodian-unavailable (lambda (r) (r))])
  (define done? #f)
  (define (do-callback obj) ; called in atomic mode
    (unless done?
      (set! done? #t)
      (callback obj)))
  (define registration
    (register-custodian-shutdown value do-callback custodian #:at-exit? at-exit?))
  (define (do-finalizer)
    (register-finalizer
     value
     (lambda (obj)
       (call-as-atomic
        (lambda ()
          (unregister-custodian-shutdown obj registration)
          (do-callback obj))))))
  (if registration
      (do-finalizer)
      (custodian-unavailable do-finalizer)))
