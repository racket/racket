#lang racket/base
;; stress tests for place-channels
(require racket/place
         racket/match
         tests/racket/place-utils)

(define (place-launch-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
    (provide place-main)

    (define (barrier ch)
      (place-channel-put ch 0)
      (place-channel-get ch))

    (define (place-main ch)
      (place-channel-put ch 2)
      (barrier ch))
  )
END
  "pct1.rkt")
  (splat
  #<<END
  (module pct2 racket
    (require racket/place)
    (provide place-main)

    (define (barrier ch)
      (place-channel-put ch 0)
      (place-channel-get ch))

    (define (place-main ch)
      (place-channel-put ch 2)
      (barrier ch))
  )
END
  "pct2.rkt")

  
  (define-values (plcnt reps symcnt)
    (match (current-command-line-arguments)
      [(vector) (values (processor-count) 10 1000000)]
      [(vector a b c) (values a b c)]))

  (define (t module-path msg)

    (let ([pls (time-n msg 0
                     (for/list ([i (in-range plcnt)]) 
                        (let ([p (dynamic-place module-path 'place-main)]) 
                          (place-channel-get p)
                          p)))])
      (barrier-m pls)
      (places-wait pls))


    (let ([pls (time-n msg 1 
                     (let ([pls (for/list ([i (in-range plcnt)])
                                  (dynamic-place module-path 'place-main))])
                        (map place-channel-get pls) pls))])
      (barrier-m pls)
      (places-wait pls)))

  (t "pct1.rkt" "racket/base")
  (t "pct2.rkt" "racket")
)

    
(place-launch-test)
