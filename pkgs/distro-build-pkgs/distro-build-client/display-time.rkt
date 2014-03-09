#lang racket/base
(require racket/date)

(provide display-time)

(define (display-time)
  (define now (seconds->date (current-seconds)))
  (printf "[~a] The time is now ~a\n" 
          (parameterize ([date-display-format 'iso-8601])
            (date->string now #t))
          (date->string now #t)))
