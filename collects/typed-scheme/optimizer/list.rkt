#lang scheme/base

(require syntax/parse
         syntax/id-table racket/dict
         unstable/match scheme/match
         (for-template scheme/base scheme/unsafe/ops)
         "../utils/utils.rkt" "../utils/tc-utils.rkt"
         (rep type-rep)
         (types abbrev type-table utils subtype)
         (optimizer utils))

(provide list-opt-expr)


(define-syntax-class list-expr
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (Listof: _)) #t]
                    [(tc-result1: (List: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class list-opt-expr
  ;; if we're iterating (with the for macros) over something we know is a list,
  ;; we can generate code that would be similar to if in-list had been used
  (pattern (#%plain-app op:id _ l)
           #:when (id-from? #'op 'make-sequence 'racket/private/for)
           #:with l*:list-expr #'l
           #:with opt
           (begin (log-optimization "in-list" #'op)
                  #'(let ((i l*.opt))
                      (values unsafe-car unsafe-cdr i
                              (lambda (x) (not (null? x)))
                              (lambda (x) #t)
                              (lambda (x y) #t))))))
