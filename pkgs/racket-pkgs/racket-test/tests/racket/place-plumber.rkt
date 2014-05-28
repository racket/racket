#lang racket/base
(require racket/place)

(define (go)
  (place 
   pch
   (plumber-add-flush! (current-plumber)
                       (lambda (e) 
                         (plumber-flush-handle-remove! e)
                         (place-channel-put pch 'done)))
   (place-channel-put pch 'ready)
   (define mode (place-channel-get pch))
   (case mode
     [(exit) (exit 2)]
     [(error)
      (error-display-handler void)
      (plumber-add-flush! (current-plumber)
                          (lambda (e)
                            (error "fail")))]
     [else (void)])))

(module+ main
  (require rackunit)

  (define p1 (go))
  (check-equal? 'ready (place-channel-get p1))
  (place-channel-put p1 'normal)
  (check-equal? 0 (place-wait p1))
  (check-equal? 'done (place-channel-get p1))

  (define p2 (go))
  (check-equal? 'ready (place-channel-get p2))
  (place-channel-put p2 'exit)
  (check-equal? 2 (place-wait p2))
  (check-equal? 'done (place-channel-get p2))
  
  (define p3 (go))
  (check-equal? 'ready (place-channel-get p3))
  (place-kill p3)
  (check-equal? 1 (place-wait p3))
  (check-equal? #f (sync/timeout 0.1 p3))

  (define p4 (go))
  (check-equal? 'ready (place-channel-get p4))
  (place-channel-put p4 'error)
  (check-equal? 1 (place-wait p4)))

(module+ test
  (require (submod ".." main)))
