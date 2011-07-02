#lang racket/base
(require racket/contract/base
         racket/dict
         "private/keyword.rkt")

(define optstx (or/c syntax? false/c))

(define checker (-> syntax? optstx any/c))

(define stxish any/c)

(define keyword-table/c
  (or/c (listof (cons/c keyword? (listof checker)))
        (and/c (not/c list?)
               dict?)))

(define options/c
  (listof (cons/c keyword? (cons/c syntax? list?))))

(provide/contract
 [parse-keyword-options
  (->* (syntax? dict?)
       (#:context optstx
        #:no-duplicates? boolean?
        #:incompatible (listof (listof keyword?))
        #:on-incompatible (-> keyword? keyword? options/c stxish optstx
                              (values options/c stxish))
        #:on-too-short (-> keyword? options/c stxish optstx
                           (values options/c stxish))
        #:on-not-in-table (-> keyword? options/c stxish optstx
                              (values options/c stxish)))
       (values options/c stxish))]
 [parse-keyword-options/eol
  (->* (syntax? dict?)
       (#:context optstx
        #:no-duplicates? boolean?
        #:incompatible (listof (listof keyword?))
        #:on-incompatible (-> keyword? keyword? options/c stxish optstx
                              (values options/c stxish))
        #:on-too-short (-> keyword? options/c stxish optstx
                           (values options/c stxish))
        #:on-not-in-table (-> keyword? options/c stxish optstx
                              (values options/c stxish))
        #:on-not-eol (-> options/c stxish optstx
                         options/c))
       options/c)]

 [options-select
  (-> options/c keyword?
      (listof list?))]
 [options-select-row
  (-> options/c keyword? #:default any/c
      any/c)]
 [options-select-value
  (-> options/c keyword? #:default any/c
      any/c)]

 [check-expression checker]
 [check-identifier checker]
 [check-stx-string checker]
 [check-stx-boolean checker]
 [check-stx-listof (-> checker checker)])
