#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/thread.rkt"
         "fd-port.rkt")

(provide make-place-ports+fds)

;; Called in atomic mode, may exit atomic mode to error
;; Given fd-port-or-falses from a parent to be used for the child
;; place, returns three fd-port-or-falses for the parent place and three
;; fds for the child place. Make sure the child fds are delivered to
;; the child place for deallocation.
(define (make-place-ports+fds in out err)
  (define-values (parent-in-fd child-in-fd)
    (if in
        (values #f (dup-fd (fd-port-fd in) void "stdin dup"))
        (reverse-pipe void "stdin pipe")))
  ;; in atomic and rktio:
  (define (clean-in)
    (rktio_close rktio child-in-fd)
    (unless in
      (rktio_close rktio parent-in-fd)))
  (define-values (parent-out-fd child-out-fd)
    (if out
        (values #f (dup-fd (fd-port-fd out) clean-in "stdout dup"))
        (pipe clean-in "stdout pipe")))
  ;; in atomic and rktio:
  (define (clean-out+in)
    (rktio_close rktio child-out-fd)
    (unless out
      (rktio_close rktio parent-out-fd))
    (clean-in))
  (define-values (parent-err-fd child-err-fd)
    (if err
        (values #f (dup-fd (fd-port-fd err) clean-out+in "stderr dup"))
        (pipe clean-out+in "stderr pipe")))
  (values (and parent-in-fd
               (open-output-fd parent-in-fd "place-in" #:is-terminal? #f))
          (and parent-out-fd
               (open-input-fd parent-out-fd "place-out"))
          (and parent-err-fd
               (open-input-fd parent-err-fd "place-err"))
          ;; Return fds for child, so they can be wrapped
          ;; in ports within the new place
          child-in-fd child-out-fd child-err-fd))

;; ----------------------------------------

;; in atomic mode
(define (dup-fd fd cleanup during)
  (define new-fd (rktioly (rktio_dup rktio fd)))
  (when (rktio-error? new-fd)
    (cleanup)
    (end-atomic)
    (raise-rktio-error 'dynamic-place new-fd (string-append "error during " during)))
  new-fd)

;; in atomic mode;
;; calls `cleanup` in atomic mode and rktio mode
(define (pipe cleanup during)
  (start-rktio)
  (define p (rktio_make_pipe rktio (bitwise-ior RKTIO_NO_INHERIT_INPUT RKTIO_NO_INHERIT_OUTPUT)))
  (when (rktio-error? p)
    (cleanup)
    (end-rktio)
    (end-atomic)
    (raise-rktio-error 'dynamic-place p (string-append "error during " during)))
  (define-values (in out) (rktio_pipe_results p))
  (rktio_free p)
  (end-rktio)
  (values in out))

(define (reverse-pipe cleanup during)
  (define-values (in out) (pipe cleanup during))
  (values out in))
