#lang racket/base
;; Parser and representation for SRV RRs.

(require racket/contract)

(provide parse-srv-rr
         (contract-out
          (struct srv-rr ((priority (integer-in 0 65535))
                          (weight (integer-in 0 65535))
                          (port (integer-in 0 65535))
                          (target string?)))))

(require racket/match)
(require "rr-generic.rkt")

(struct srv-rr (priority
                weight
                port
                target)
  #:prefab)

(define (parse-srv-rr answer-records reply)
  (let loop ((rrs answer-records))
    (match rrs
      ['() '()]
      [(cons rr rest)
       (match (rr-data rr)
         [(list* prio1 prio2 weight1 weight2 port1 port2 target-bytes)
          (define-values (target-name _rest) (parse-name target-bytes reply))
          (cons (srv-rr (octet-pair->number prio1 prio2)
                        (octet-pair->number weight1 weight2)
                        (octet-pair->number port1 port2)
                        (bytes->string/latin-1 target-name))
                (loop rest))]
         [_ (loop rest)])])))
