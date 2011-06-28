#lang racket/base
(require web-server/http/response-structs
         web-server/http/xexpr
         racket/list)

(define response/basic? response?)
(define (make-response/basic c m s mime hs)
  (response/full c m s mime hs #""))
(define response/basic-code response-code)
(define response/basic-message response-message)
(define response/basic-seconds response-seconds)
(define response/basic-mime response-mime)
(define response/basic-headers response-headers)

(define BODIES (make-weak-hasheq))
(define response/full? response?)
(define (make-response/full c m s mime hs bs)
  (define r (response/full c m s mime hs bs))
  (hash-set! BODIES r bs)
  r)
(define response/full-code response-code)
(define response/full-message response-message)
(define response/full-seconds response-seconds)
(define response/full-mime response-mime)
(define response/full-headers response-headers)
(define (response/full-body r)
  (hash-ref BODIES r))

(define GENS (make-weak-hasheq))
(define response/incremental? response?)
(define (make-response/incremental c m s mime hs gen)
  (define r
    (response c m s mime hs
              (λ (out)
                (gen (λ bss
                       (for ([bs (in-list bss)])
                         (write-bytes bs out)))))))
  (hash-set! GENS r gen)
  r)
(define response/incremental-code response-code)
(define response/incremental-message response-message)
(define response/incremental-seconds response-seconds)
(define response/incremental-mime response-mime)
(define response/incremental-headers response-headers)
(define (response/incremental-body r)
  (hash-ref GENS r))

(define make-xexpr-response response/xexpr)

(define (normalize-response r [close? #f])
  (cond
    [(response? r) r]
    [(and (pair? r) (bytes? (car r)))
     (response/full 200 #"Okay" (current-seconds) (car r)
                    empty
                    (map (λ (x) (if (bytes? x) x (string->bytes/utf-8 x)))
                         (cdr r)))]
    [else
     (response/xexpr r)]))

(provide 
 (except-out (all-defined-out)
             BODIES
             GENS))
