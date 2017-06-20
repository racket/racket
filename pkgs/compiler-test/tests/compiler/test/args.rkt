#lang racket

(module multi-test racket/base
  (require racket/match)
  (match (current-command-line-arguments)
    [(vector "1" "2") 1]
    [_ (void)]))

(module+ test
  (match (current-command-line-arguments)
    [(vector "1") 1]
    [_ (void)]))
