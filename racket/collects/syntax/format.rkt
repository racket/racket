#lang racket/base

(require racket/contract/base
         racket/list
         syntax/srcloc
         "private/format.rkt")

(provide (contract-out
          [~id (->* [] [#:context (or/c syntax? #f)
                        #:source source-location?
                        #:props (or/c syntax? #f 'infer)
                        #:track (or/c #t (-> identifier? identifier?) #f)]
                    #:rest (listof piece/c)
                    identifier?)]
          [~id/1 (->* [] [#:context (or/c syntax? #f 'infer)
                          #:source (or/c source-location? 'infer)
                          #:props (or/c syntax? #f 'infer)
                          #:track (or/c #t (-> identifier? identifier?) #f)]
                      #:rest (and/c (listof piece/c) list-contains-exactly-one-identifier?)
                      identifier?)]
          [~symbol (-> piece/c ... symbol?)]))

(define piece/c (or/c identifier? string? symbol? keyword? char? number?))

(define (list-contains-exactly-one-identifier? lst)
  (= (count identifier? lst) 1))
