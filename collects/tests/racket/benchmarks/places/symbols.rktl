#lang racket/base
;; stress tests for place-channels communciating symbols
(require racket/place
         racket/match
         "place-utils.rkt")

(define (symbol-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place
             racket/match
             "place-utils.rkt")
    (provide place-main)

    (define (place-main ch)
      (match (place-channel-recv ch)
        [(list id reps cnt)
          (define ids (number->string id))
          (for ([j (in-range reps)])
            (define repstr (number->string j))
            (barrier ch)
            (for ([i (in-range cnt)])
              (string->symbol (string-append ids "_" repstr "_" (number->string i))))
            (barrier ch)
)]))
  )
END
  "pct1.ss")

  (define-values (plcnt reps symcnt)
    (match (current-command-line-arguments)
      [(vector) (values (processor-count) 10 1000000)]
      [(vector a b c) (values a b c)]))

  (define pls (for/list ([i (in-range plcnt)]) (place "pct1.ss" 'place-main)))

  (for ([i (in-range plcnt)]
        [pl pls])
    (place-channel-send pl (list i reps symcnt)))
  (for ([j (in-range reps)])
    (time-n "1million-symbols" j
      (barrier-m pls)
      (barrier-m pls))))

(define (symbol-read-test)
  (splat
  #<<END
  (module pct1 racket/base
    (require racket/place
             "place-utils.rkt")
    (provide place-main)
    (require algol60/parse
             (for-syntax racket/base
                         syntax/parse)
             syntax/parse
             racket/class)

    (define (place-main ch)
            (barrier ch))
  )
END
  "pct2.ss")

  (define-values (plcnt reps symcnt)
    (match (current-command-line-arguments)
      [(vector) (values (processor-count) 4 1000000)]
      [(vector a b c) (values a b c)]))


  (for ([j (in-range reps)])
    (time-n "require-algol-parse/racket-class" j
      (define pls (for/list ([i (in-range plcnt)]) (place "pct2.ss" 'place-main)))
      (barrier-m pls))))

(symbol-test)
(symbol-read-test)
