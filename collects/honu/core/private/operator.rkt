#lang racket/base

(require (for-syntax racket/base
                     "transformer.rkt"
                     "fixture.rkt"
                     syntax/parse)
         (only-in "literals.rkt" %racket))

(provide (all-defined-out))

(define-syntax (define-honu-operator/syntax stx)
  (syntax-parse stx
    [(_ name precedence associativity binary-function)
     #'(define-syntax name (make-honu-operator precedence associativity binary-function #f))]
    [(_ name precedence associativity binary-function unary-function)
     #'(define-syntax name (make-honu-operator precedence associativity binary-function unary-function))]))

(define-syntax (define-honu-fixture stx)
  (syntax-parse stx
    [(_ name transformer)
     #'(define-syntax name (make-fixture transformer))]))

(define-syntax-rule (define-binary-operator name precedence associativity operator)
    (define-honu-operator/syntax name precedence associativity
                                 ;; binary
                                 (lambda (left right)
                                   (with-syntax ([left left]
                                                 [right right])
                                     #'(%racket (operator left right))))
                                 ;; unary
                                 (lambda (argument)
                                   (with-syntax ([argument argument])
                                     #'(%racket (operator argument))))))

(define-syntax-rule (define-unary-operator name precedence associativity operator)
                      (define-honu-operator/syntax name precedence associativity
                                                   #f
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument argument])
                                                       #'(%racket (operator argument))))))

(define-honu-operator/syntax honu-flow 0.001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(%racket (right left)))))

(begin-for-syntax
  (define-syntax-rule (mutator change)
                      (lambda (left right)
                        (with-syntax ([left left]
                                      [right (change left right)])
                          #'(%racket (set! left right))))))

;; Traditional assignment operator
(define-honu-operator/syntax honu-equal 0.0001 'left
  (mutator (lambda (left right) right)))

(define-syntax-rule (define-honu-operator-= name operation)
  (define-honu-operator/syntax name 0.0001 'left
                               (mutator (lambda (left right)
                                          (with-syntax ([left left] [right right])
                                            #'(operation left right))))))

;; Operators that mutate the left hand side
(define-honu-operator-= honu-+= +)
(define-honu-operator-= honu--= -)
(define-honu-operator-= honu-*= *)
(define-honu-operator-= honu-/= /)

(define-binary-operator honu-+ 1 'left +)
(define-binary-operator honu-- 1 'left -)
(define-binary-operator honu-* 2 'left *)
(define-binary-operator honu-/ 2 'left /)
(define-binary-operator honu-^ 2 'right expt)
(define-binary-operator honu-< 0.9 'left <)
(define-binary-operator honu-<= 0.9 'left <=)
(define-binary-operator honu-> 0.9 'left >)
(define-binary-operator honu->= 0.9 'left >=)
;; (define-binary-operator honu-= 0.9 'left =)
(define-binary-operator honu-and 0.5 'left and)
(define-binary-operator honu-or 0.5 'left or)
(define-binary-operator honu-cons 0.1 'right cons)
(define-binary-operator honu-map 0.09 'left map)
(define-binary-operator honu-string=? 1 'left string=?)
(define-binary-operator honu-modulo 2 'left modulo)

(define-binary-operator honu-to 0.001 'left
                        (lambda (left right)
                          (for/list ([i (in-range left right)]) i)))

(define-unary-operator honu-not 0.7 'left not)

(define-binary-operator honu-== 1 'left equal?)
(define-binary-operator honu-not-equal 1 'left (lambda (left right)
                                                 (not (equal? left right))))
