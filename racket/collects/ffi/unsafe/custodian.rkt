#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic)

(provide (protect-out register-custodian-shutdown
                      register-finalizer-and-custodian-shutdown
                      unregister-custodian-shutdown))

(define _Scheme_Custodian_Reference-pointer
  (_gcable (_cpointer 'Scheme_Custodian_Reference)))

(define scheme_add_managed
  (get-ffi-obj 'scheme_add_managed #f
               (_fun _racket _racket _fpointer _racket _int
                     -> _Scheme_Custodian_Reference-pointer)))
(define scheme_add_managed_close_on_exit
  (get-ffi-obj 'scheme_add_managed_close_on_exit #f
               (_fun _racket _racket _fpointer _racket
                     -> _Scheme_Custodian_Reference-pointer)))

(define scheme_remove_managed
  (get-ffi-obj 'scheme_remove_managed #f
               (_fun _Scheme_Custodian_Reference-pointer _racket -> _void)))

(define (shutdown-callback impl proc+box) 
  ((car proc+box) impl))
(define shutdown-callback-box (box #f))
(define shutdown_callback
  (cast shutdown-callback (_fun #:atomic? #t #:keep shutdown-callback-box
                                _racket _racket -> _void) _fpointer))

(define (register-custodian-shutdown obj proc [custodian (current-custodian)]
                                     #:at-exit? [at-exit? #f]
                                     #:weak? [weak? #f])
  (define proc+box (cons proc
                          shutdown-callback-box)) ; proc as data -> ffi callback retained
  (if at-exit?
      (scheme_add_managed_close_on_exit custodian
                                        obj shutdown_callback proc+box)
      (scheme_add_managed custodian
                          obj shutdown_callback proc+box
                          (if weak? 0 1))))

(define (unregister-custodian-shutdown obj mref)
  (scheme_remove_managed mref obj))

(define (register-finalizer-and-custodian-shutdown value callback
                                                   [custodian (current-custodian)]
                                                   #:at-exit? [at-exit? #f])
  (define done? #f)
  (define (do-callback obj) ; called in atomic mode
    (unless done?
      (set! done? #t)
      (callback obj)))
  (define registration
    (register-custodian-shutdown value do-callback custodian #:at-exit? at-exit?))
  (register-finalizer value
    (lambda (obj)
      (call-as-atomic
       (lambda ()
         (unregister-custodian-shutdown obj registration)
         (do-callback obj))))))
