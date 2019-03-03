#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/place-local.rkt"
         "logger.rkt"
         "level.rkt"
         "wanted.rkt"
         "receiver.rkt")

(provide logger?
         logger-name
         current-logger
         make-logger
         log-level?
         log-level?*   ; ok to call in host-Scheme interrupt handler
         log-max-level
         log-all-levels
         log-level-evt
         log-message
         log-message*  ; ok to call in host-Scheme interrupt handler
         log-receiver?
         make-log-receiver
         add-stderr-log-receiver!
         add-stdout-log-receiver!
         add-syslog-log-receiver!
         logger-init!)

(define (make-root-logger)
  (create-logger #:topic #f #:parent #f #:propagate-filters 'none))

(define-place-local root-logger (make-root-logger))

(define current-logger
  (make-parameter root-logger
                  (lambda (l)
                    (unless (logger? l)
                      (raise-argument-error 'current-logger "logger?" l))
                    l)))

(define (logger-init!)
  (set! root-logger (make-root-logger))
  (current-logger root-logger))

(define (make-logger [topic #f] [parent #f] . filters)
  (unless (or (not topic) (symbol? topic))
    (raise-argument-error 'make-logger "(or/c symbol? #f)" topic))
  (unless (or (not parent) (logger? parent))
    (raise-argument-error 'make-logger "(or/c logger? #f)" parent))
  (create-logger #:topic topic
                 #:parent parent
                 #:propagate-filters (parse-filters 'make-logger filters #:default-level 'debug)))

;; Can be called in any host Scheme thread, including in an interrupt
;; handler (where "interrupt" is a host-Scheme concept, such as a GC
;; handler). If it's not the thread that runs Racket, then it's in
;; atomic, non-interrupt mode and we assume that the argument checks
;; will pass.
(define/who (log-level? logger level [topic #f])
  (check who logger? logger)
  (check-level who level)
  (check who #:or-false symbol? topic)
  (atomically/no-interrupts/no-wind
   (log-level?* logger level topic)))

;; In atomic mode with interrupts disabled
(define/who (log-level?* logger level topic)
  (level>=? (logger-wanted-level logger topic) level))

(define/who (log-max-level logger [topic #f])
  (check who logger? logger)
  (check who #:or-false symbol? topic)
  (level->user-representation
   (atomically/no-interrupts/no-wind
    (logger-wanted-level logger topic))))

(define/who (log-all-levels logger)
  (check who logger? logger)
  (logger-all-levels logger))

(define/who (log-level-evt logger)
  (check who logger? logger)
  (define s
    (atomically
     (cond
       [(unbox (logger-level-sema-box logger))
        => (lambda (s) s)]
       [else
        (define s (make-semaphore))
        (set-box! (logger-level-sema-box logger) s)
        s])))
  (semaphore-peek-evt s))

(define/who log-message
  ;; Complex dispatch based on number and whether third is a string:
  (case-lambda
    [(logger level message)
     (define topic (and (logger? logger) (logger-name logger)))
     (do-log-message who logger level topic message #f #t)]
    [(logger level topic/message message/data)
     (cond
       [(string? topic/message)
        (define topic (and (logger? logger) (logger-name logger)))
        (do-log-message who logger level topic topic/message message/data #t)]
       [(or (not topic/message) (symbol? topic/message))
        (do-log-message who logger level topic/message message/data #f #t)]
       [else
        (check who logger? logger)
        (check-level who level)
        (raise-argument-error who "(or/c string? symbol?)" topic/message)])]
    [(logger level topic/message message/data data/prefix?)
     (cond
       [(string? topic/message)
        (define topic (and (logger? logger) (logger-name logger)))
        (do-log-message who logger level topic topic/message message/data data/prefix?)]
       [(or (not topic/message) (symbol? topic/message))
        (do-log-message who logger level topic/message message/data data/prefix? #t)]
       [else
        (check who logger? logger)
        (check-level who level)
        (raise-argument-error who "(or/c string? symbol?)" topic/message)])]
    [(logger level topic message data prefix?)
     (do-log-message who logger level topic message data prefix?)]))

(define (do-log-message who logger level topic message data prefix?)
  (check who logger? logger)
  (check-level who level)
  (check who #:or-false symbol? topic)
  (check who string? message)
  (atomically/no-interrupts/no-wind
   (log-message* logger level topic message data prefix? #f)))

;; In atomic mode with interrupts disabled
;; Can be called in any host Scheme thread and in interrupt handler,
;; like `log-level?*`
(define (log-message* logger level topic message data prefix? in-interrupt?)
  (define msg #f)
  (when ((logger-max-wanted-level logger) . level>=? . level)
    (let loop ([logger logger])
      (for ([r (in-list (logger-receivers logger))])
        (when ((filters-level-for-topic (log-receiver-filters r) topic) . level>=? . level)
          (unless msg
            (set! msg (vector-immutable
                       level
                       (string->immutable-string
                        (if (and prefix? topic)
                            (string-append (symbol->string topic)
                                           ": "
                                           message)
                            message))
                       data
                       topic)))
          (log-receiver-send! r msg in-interrupt?)))
      (let ([parent (logger-parent logger)])
        (when (and parent
                   ((filters-level-for-topic (logger-propagate-filters logger) topic) . level>=? . level))
          (loop parent))))))
