#lang racket/base

(provide load-configure-expand
         set-load-configure-expand!
         enter-configure-parameterization
         call-with-configure-parameterization)

(define load-configure-expand (lambda (mpi ns)
                                (values current-parameterization
                                        current-parameterization)))

(define (set-load-configure-expand! proc)
  (set! load-configure-expand proc))

(define exit-parameteterization-key (gensym 'exit-paramz))

(define (current-exit-parameterization)
  (or (continuation-mark-set-first #f exit-parameteterization-key)
      current-parameterization))

(define (enter-configure-parameterization enter)
  (call-with-parameterization
   (check 'exit-parameterization ((current-exit-parameterization)))
   (lambda ()
     (check 'enter-parameterization (enter)))))

(define (call-with-configure-parameterization paramz exit-paramz thunk)
  (with-continuation-mark
    exit-parameteterization-key
    exit-paramz
    (call-with-parameterization
     paramz
     thunk)))

(define (check who what)
  (unless (parameterization? what)
    (raise-result-error who
                        "parameterization?"
                        what))
  what)
