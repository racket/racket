#lang racket/base
(require syntax/parse syntax/parse/experimental/contract
         (for-template racket/base
                       racket/contract
                       slideshow/pict
                       "ppict.rkt"))
(provide fragment)

(define-splicing-syntax-class (fragment who)
  #:description (format "~a fragment" who)
  (pattern (~seq #:go pl)
           #:declare pl (expr/c #'placer? #:name "argument to #:go")
           #:with code #'(p:go pl.c))
  (pattern (~seq #:next)
           #:with code #'(p:out))
  (pattern (~seq e)
           #:declare e (expr/c #'(or/c pict? real? #f) #:name "element")
           #:with code #'(p:elem e.c)))
