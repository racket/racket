#lang racket/base
(require "link.rkt"
         "linklet-info.rkt")

(provide needed!)

;; Compute which linklets are actually used as imports
(define (needed! lnk reason
                 #:seen seen
                 #:needed needed)
  (let needed! ([lnk lnk] [reason reason])
    (unless (hash-ref needed lnk #f)
      (define li (hash-ref seen lnk #f))
      (when li
        (hash-set! needed lnk reason)
        (for ([in-lnk (in-list (linklet-info-imports li))])
          (needed! in-lnk lnk))))))
