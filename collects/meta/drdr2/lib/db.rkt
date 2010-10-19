#lang racket/base
(require racket/match)

(define (db-connect spec)
  #f)

(define (db-ref db . path)
  #f)

(define db-set!
  (match-lambda*
    [(list db path ... value)
     #f]))

(define (db-close! db)
  #f)

(provide (all-defined-out))