#lang racket/base

(require honu/core/private/syntax
         honu/core/private/honu-typed-scheme
         honu/core/private/literals
         racket/list
         racket/match
         (for-syntax racket/base
                     honu/core/private/literals
                     honu/core/private/parse2
                     honu/core/private/compile
                     syntax/parse))

(define-literal+set linq-literals
                    linq-from linq-select linq-where
                    linq-order-by)
(provide linq (rename-out [linq-from from]
                          [linq-where where]
                          [linq-order-by orderby]
                          [linq-select select]))

(define-honu-syntax linq
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft linq-literals)
                       #:literals (honu-in)
      [(_ linq-from name:id honu-in
          (~var store honu-expression)
          (~optional (~seq linq-where where:honu-expression))
          (~optional (~seq linq-order-by order-by:honu-expression))
          linq-select select:honu-expression . rest)
       (define out
         (with-syntax ([(guard ...)
                        (if (attribute where)
                          #'(#:when where.result)
                          #'())]
                       [order (if (attribute order-by)
                                #'(sort store.result string<?
                                        #:key (lambda (name) order-by.result))
                                #'store.result)])
           (racket-syntax (for/list ([name order]
                                     guard ...)
                            select.result))))
       (values out #'rest #f)])))
          
#|
var addresses = linq from add in xml.Descendants("Table1")
                     select new {
                       Name = add.Element("familyName").Value,
                       Address = add.Element("address").Value
                     };

|#

(provide find-descendants)
(define (find-descendants xexpr name)
  (match xexpr
    [(list root root-attributes elements ...)
     (filter (lambda (element)
               (match element
                 [(list (? (lambda (x) (string=? name (symbol->string x)))) stuff ...) #t]
                 [else #f]))
             elements)]))

(provide get-element)
(define (get-element xexpr name)
  (first (find-descendants xexpr name)))

(provide get-text)
(define (get-text xexpr)
  (match xexpr
    [(list name attributes text)
     text]))
