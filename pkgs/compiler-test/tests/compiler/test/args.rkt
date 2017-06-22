#lang racket

(module+ test
  (match (current-command-line-arguments)
    [(vector "1") 1]
    [_ (void)]))
