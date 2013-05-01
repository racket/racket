#lang racket/base
(require racket/contract
         "lang-struct.rkt")

(provide 
 (contract-out
  [lang-enumerators (-> (listof nt?) (hash/c symbol? enum?))]
  [pat-enumerator (-> (hash/c symbol? enum?)
                      any/c ;; pattern
                      enum?)]
  [enum-ith (-> enum? exact-nonnegative-integer? any/c)]
  [enum? (-> any/c boolean?)]))

(struct enum ())

(define (lang-enumerators nts) (make-hash))
(define (pat-enumerator li p)
  (unless (equal? p '(name natural natural))
    (error 'enum.rkt "not yet implemented"))
  (enum))
(define (enum-ith e i) i)
