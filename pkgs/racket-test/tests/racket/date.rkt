#lang racket/base
(require racket/date rackunit)
;; test that German hours & minutes are separated with a colon, not a period
(check-equal? (parameterize ([date-display-format 'german])
                (date->string
                 (seconds->date
                  (find-seconds 30 45 2 1 1 1970))
                 #t))
              "1. Januar 1970, 02:45")
