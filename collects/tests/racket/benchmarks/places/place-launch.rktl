#lang racket/base
;; stress tests for place-channels
(require racket/place
         racket/match
         "place-utils.rkt")

(define (place-launch-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place)
    (provide place-main)

    (define (barrier ch)
      (place-channel-send ch 0)
      (place-channel-recv ch))

    (define (place-main ch)
      (place-channel-send ch 2)
      (barrier ch))
  )
END
  "pct1.ss")
  (splat
  #<<END
  (module pct2 racket
    (require racket/place)
    (provide place-main)

    (define (barrier ch)
      (place-channel-send ch 0)
      (place-channel-recv ch))

    (define (place-main ch)
      (place-channel-send ch 2)
      (barrier ch))
  )
END
  "pct2.ss")

  
  (define-values (plcnt reps symcnt)
    (match (current-command-line-arguments)
      [(vector) (values (processor-count) 10 1000000)]
      [(vector a b c) (values a b c)]))

  (define (t module-path msg)

    (let ([pls (time-n msg 0
                     (for/list ([i (in-range plcnt)]) 
                        (let ([p (place module-path 'place-main)]) 
                          (place-channel-recv p)
                          p)))])
      (barrier-m pls)
      (places-wait pls))


    (let ([pls (time-n msg 1 
                     (let ([pls (for/list ([i (in-range plcnt)])
                                  (place module-path 'place-main))])
                        (map place-channel-recv pls) pls))])
      (barrier-m pls)
      (places-wait pls)))

  (t "pct1.ss" "racket/base")
  (t "pct2.ss" "racket")
)

    
(place-launch-test)
