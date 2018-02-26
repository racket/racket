
(define (place-enabled?)
  #f)

(define (place? v)
  #f)

(define (place-channel? v)
  #f)

(define place-specific-table (make-hasheq))

(define (unsafe-get-place-table)
  place-specific-table)

(define-syntax define-place-not-yet-available
  (syntax-rules ()
    [(_ id)
     (define (id . args)
       (error 'id "place API not yet supported"))]
    [(_ id ...)
     (begin (define-place-not-yet-available id) ...)]))

(define-place-not-yet-available
  place-break
  place-channel-get
  place-channel-put
  place-sleep
  place-channel
  place-dead-evt
  place-kill
  place-message-allowed?
  dynamic-place
  place-wait
  place-pumper-threads
  place-shared?)
