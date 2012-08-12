#lang racket/base
(require ffi/unsafe)

(provide (protect-out register-custodian-shutdown
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

(define (shutdown-callback impl proc+self) 
  ((car proc+self) impl))
(define shutdown_callback
  (cast shutdown-callback (_fun #:atomic? #t _racket _racket -> _void) _fpointer))

(define (register-custodian-shutdown obj proc [custodian (current-custodian)]
                                     #:atexit? [atexit? #f]
                                     #:weak? [weak? #f])
  (define proc+self (cons proc
                          shutdown-callback)) ; proc as data -> ffi callback retained
  (if atexit?
      (scheme_add_managed_close_on_exit custodian
                                        obj shutdown_callback proc+self)
      (scheme_add_managed custodian
                          obj shutdown_callback proc+self
                          (if weak? 0 1))))

(define (unregister-custodian-shutdown obj mref)
  (scheme_remove_managed mref obj))
