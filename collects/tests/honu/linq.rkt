#lang racket/base

(require honu/core/private/macro2
         honu/core/private/honu-typed-scheme
         honu/core/private/literals
         racket/list
         racket/match
         (for-syntax racket/base
                     honu/core/private/literals
                     honu/core/private/parse2
                     syntax/parse))

(define-literal linq-from linq-select)
(provide linq (rename-out [linq-from from]
                          [linq-select select]))

(define-honu-syntax linq
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-in)
      [(_ linq-from name:id honu-in
          (~var store honu-expression)
          linq-select what:honu-expression . rest)
       (define out
         #'(for/list ([name store.result])
             what.result))
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
