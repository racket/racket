#lang racket/base
(require racket/port
         racket/list
         net/url)
(module+ test
  (require rackunit)
  (define url-no-slash
    "http://www.racket-lang.org")
  (define (test u)
    (define r
      (first
       (port->lines
        (head-impure-port
         (string->url u)))))
    (printf "~v => ~v\n" u r)
    r)
  (define no-slash-result (test url-no-slash))
  (define slash-result (test (format "~a/" url-no-slash)))
  (check-equal? no-slash-result slash-result))
