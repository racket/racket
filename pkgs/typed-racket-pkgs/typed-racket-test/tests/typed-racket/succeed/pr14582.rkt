#lang typed/racket/base
(define-type CodeOfBoolean (Code Boolean))
(define-type CodeOfInteger (Code Integer))
(define-type CodeOfAny
  (U CodeOfInteger
     CodeOfBoolean))

(define-type (Code Type)
  (U Type
     (If Type)
     (Begin Type)))


(struct (Type) If
  ([cond : CodeOfBoolean]
   [then : (Code Type)]
   [else : (Code Type)])
  #:transparent)



(define-type (ListEndingIn ListType EndType)
  (U (Pair EndType Null)
     (Pair ListType (ListEndingIn ListType EndType))))

(struct (Type) Begin
  ([exprs : (ListEndingIn CodeOfAny (Code Type))]))


(define QuotedCode : CodeOfInteger
  (If #t
      1
      (Begin
       (list 2 #f (If #t 3 4)))))
