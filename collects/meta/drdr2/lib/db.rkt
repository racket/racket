#lang racket/base
(require racket/match
         racket/contract)

(struct db ())

(define (db-connect spec)
  (db))

(define (db-ref db . path)
  #f)

(define (db-set! db value . path)
  #f)

(define (db-close! db)
  #f)

(provide/contract
 [db? (any/c . -> . boolean?)]
 [db-connect (string? . -> . db?)]
 [db-ref ((db?) () #:rest (listof string?) . ->* . any/c)] 
 [db-set! ((db? any/c) () #:rest (listof string?) . ->* . void)]
 [db-close! (db? . -> . void)])