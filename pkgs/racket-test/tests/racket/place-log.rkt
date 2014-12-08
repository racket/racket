#lang racket/base
(require racket/place
         rackunit)

(define-struct place-event (place-id what value time)
    #:prefab)

(define r (make-log-receiver (current-logger) 'debug 'place))

(define (check-log kind [expected-value (void)])
  (let ([v (vector-ref (sync r) 2)])
    (check-equal? #t (place-event? v))
    (check-equal? #t (exact-integer? (place-event-place-id v)))
    (check-equal? kind (place-event-what v))
    (unless (void? expected-value )
      (check-equal? expected-value (place-event-value v)))
    (place-event-value v)))

(define-values (i o) (place-channel))

(place-channel-put o 17)
(void (check-log 'put))

(check-equal? 17 (place-channel-get i))
(void (check-log 'get))

(void (place-wait (dynamic-place 'racket/base 'values)))
(define v (check-log 'create))
(void (check-log 'reap v))
