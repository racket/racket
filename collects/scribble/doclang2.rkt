#lang racket/base

;; A slightly nicer version of doclang where the parameters are keyword-based
;; rather than positional.  Delegates off to the original doclang.

(require (prefix-in doclang: "doclang.rkt")
         (for-syntax racket/base
                     syntax/parse)) 

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [*module-begin #%module-begin]))

;; Module wrapper ----------------------------------------

(define-syntax (*module-begin stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:id id))
             (~optional (~seq #:post-process post-process))
             (~optional (~seq #:exprs exprs)))
        ...
        . body)
     (with-syntax ([id (or (attribute id) 
                           #'doc)]
                   [post-process (or (attribute post-process)
                                     #'values)]
                   [exprs (or (attribute exprs)
                              #'())])
       #'(doclang:#%module-begin id post-process exprs . body))]))
