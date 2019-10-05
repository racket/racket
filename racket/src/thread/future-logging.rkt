#lang racket/base
(require "host.rkt"
         "parameter.rkt"
         "future-object.rkt"
         "place-local.rkt")

(provide log-future
         logging-futures?
         flush-future-log

         init-future-logging-place!
         install-future-logging-procs!)

(struct future-event (future-id proc-id action time prim-name user-data)
  #:prefab)

(define-place-local events (box null))

(define (init-future-logging-place!)
  (set! events (box null)))

;; called with no future locks held
(define (log-future action [future-id #f]
                    #:prim-name [prim-name #f]
                    #:data [data #f])
  (cond
    [(current-future)
     => (lambda (me-f)
          (define e (future-event (or future-id (future*-id me-f))
                                  (get-pthread-id)
                                  action
                                  (current-inexact-milliseconds)
                                  prim-name
                                  data))
          (let loop ()
            (define old-events (unbox events))
            (unless (box-cas! events old-events (cons e old-events))
              (loop))))]
    [(logging-futures?)
     (flush-future-log)
     (define id (or future-id
                    (let ([f (currently-running-future)])
                      (and f
                           (future*-id f)))))
     (log-future-event* (future-event id 0 action (current-inexact-milliseconds) prim-name data))]))

;; maybe in atomic mode and only in main pthread
(define (logging-futures?)
  (logging-future-events?))

;; in atomic mode and only in main pthread
(define (flush-future-log)
  (define new-events (unbox* events))
  (unless (null? new-events)
    (cond
      [(box-cas! events new-events null)
       (when (logging-futures?)
         (for ([e (in-list (reverse new-events))])
           (log-future-event* e)))]
      [else (flush-future-log)])))

(define (log-future-event* e)
  (define proc-id (future-event-proc-id e))
  (define action (future-event-action e))
  (define msg (string-append "id "
                             (let ([id (future-event-future-id e)])
                               (if id
                                   (number->string (future-event-future-id e))
                                   "-1"))
                             ", process "
                             (number->string proc-id)
                             ": "
                             (if (and (eqv? proc-id 0)
                                      (or (eq? action 'block)
                                          (eq? action 'sync)))
                                 (string-append (if (eq? action 'block)
                                                    "HANDLING: "
                                                    "synchronizing: ")
                                                (symbol->string
                                                 (or (future-event-prim-name e)
                                                     '|[unknown]|)))
                                 (action->string action))
                             "; time: "
                             (number->string (future-event-time e))))
  (log-future-event msg e))

(define (action->string a)
  (case a
    [(create) "created"]
    [(complete) "completed"]
    [(start-work) "started work"]
    [(end-work) "ended work"]
    [(block) "BLOCKING on process 0"]
    [(sync) "synchronizing with process 0"]
    [(touch) "touching future: touch"]
    [(result) "result determined"]
    [(suspend) "suspended"]
    [(touch-pause) "paused for touch"]
    [(touch-resume) "resumed for touch"]
    [else "[unknown action]"]))

;; ----------------------------------------

(define logging-future-events? (lambda () #f))
(define log-future-event (lambda (msg e) (void)))

(define (install-future-logging-procs! logging? log)
  (set! logging-future-events? logging?)
  (set! log-future-event log))
