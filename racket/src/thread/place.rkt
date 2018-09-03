#lang racket/base
(require (only-in '#%unsafe unsafe-abort-current-continuation/no-wind)
         "place-local.rkt"
         "check.rkt"
         "host.rkt"
         "schedule.rkt"
         "atomic.rkt"
         "thread.rkt"
         "custodian.rkt"
         "plumber.rkt"
         "exit.rkt"
         "sync.rkt"
         "evt.rkt")

(provide dynamic-place
         place?
         place-break
         place-kill
         place-wait
         place-dead-evt
         place-sleep

         place-channel
         place-channel? 
         place-channel-get
         place-channel-put
         place-message-allowed?

         place-pumper-threads
         place-shared?
         unsafe-add-post-custodian-shutdown)

;; ----------------------------------------

(struct place ([result #:mutable]
               custodian
               [post-shutdown #:mutable]
               pumper-threads))

(define-place-local current-place #f)

(define place-prompt-tag (make-continuation-prompt-tag 'place))

(define (dynamic-place path sym in out err)
  (define c (make-custodian))
  (define new-place (place #f                   ; result
                           c
                           '()                  ; post-shutdown
                           (make-vector 3 #f))) ; pumper-threads
  (define orig-plumber (make-plumber))
  (define (default-exit v)
    (plumber-flush-all orig-plumber)
    (unsafe-abort-current-continuation/no-wind
     place-prompt-tag
     (if (byte? v) v 0)))
  (host:fork-place
   (lambda ()
     (define finish (host:start-place path sym in out err))
     (call-in-main-thread
      (lambda ()
        (define result
          (call-with-continuation-prompt
           (lambda ()
             (set! current-place new-place)
             (current-custodian c)
             (current-plumber orig-plumber)
             (exit-handler default-exit)
             (finish
              (lambda (in-th out-th err-th)
                (vector-set! (place-pumper-threads place) 0 in-th)
                (vector-set! (place-pumper-threads place) 1 out-th)
                (vector-set! (place-pumper-threads place) 2 err-th)))
             (default-exit 0))
           place-prompt-tag
           (lambda (v) v)))
        (set-place-result! new-place result)))))
  new-place)

(define/who (place-break p [kind #f])
  (check who place? p)
  (unless (or (not kind) (eq? kind 'hangup) (eq? kind 'terminate))
    (raise-argument-error who "(or/c #f 'hangup 'terminate)" kind))
  (void))

(define/who (place-kill p)
  (check who place? p)
  (void))

(define/who (place-wait p)
  (check who place? p)
  (sync never-evt))

(define/who (place-dead-evt p)
  (check who place? p)
  never-evt)

(define/who (place-sleep msecs)
  (void))

;; ----------------------------------------

(struct pchannel ()
  #:reflection-name 'place-channel)

(define (place-channel? v)
  (pchannel? v))

(define (place-channel)
  (values (pchannel)
          (pchannel)))

(define (place-channel-get pch)
  (sync never-evt))

(define (place-channel-put pch v)
  (void))

(define (place-message-allowed? v)
  #t)

;; ----------------------------------------

(define (place-shared? v)
  #f)

(define (unsafe-add-post-custodian-shutdown proc)
  (atomically
   (set-place-post-shutdown! current-place
                             (cons proc
                                   (place-post-shutdown current-place)))))
