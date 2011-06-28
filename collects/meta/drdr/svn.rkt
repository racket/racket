#lang racket

(define-struct svn-rev () #:prefab)
(define-struct (svn-rev-nolog svn-rev) () #:prefab)
(define-struct (svn-rev-log svn-rev) (num author date msg changes) #:prefab)
(define-struct svn-change (action path) #:prefab)

(provide/contract
 [struct svn-rev ()]
 [struct (svn-rev-nolog svn-rev) ()]
 [struct (svn-rev-log svn-rev)
         ([num exact-nonnegative-integer?]
          [author string?]
          [date string?]
          [msg string?]
          [changes (listof svn-change?)])]
 [struct svn-change 
         ([action symbol?]
          [path path-string?])])
