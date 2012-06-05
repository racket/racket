#lang racket/base

(require (for-syntax racket/base
                     "transformer.rkt"
                     "fixture.rkt"
                     "compile.rkt"
                     "debug.rkt"
                     syntax/parse))

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
                                     (racket-syntax (operator left right))))
                                 ;; unary
                                 (lambda (argument)
                                   (with-syntax ([argument (honu->racket argument)])
                                     (racket-syntax (operator argument))))))

(define-syntax-rule (define-unary-operator name precedence associativity operator)
                      (define-honu-operator/syntax name precedence associativity
                                                   #f
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument (honu->racket argument)])
                                                       (racket-syntax (operator argument))))))

(define-honu-operator/syntax honu-flow 0.001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      (racket-syntax (right left)))))

(begin-for-syntax
  (define-syntax-rule (mutator change)
                      (lambda (left right)
                        (with-syntax ([left (honu->racket left)]
                                      [right (change left (honu->racket right))])
                          (racket-syntax (set! left right))))))

;; Traditional assignment operator
(define-honu-operator/syntax honu-equal 0.0001 'left
  (let ([plain-mutate (mutator (lambda (left right) right))])
    (lambda (left right)
      (define mutate (syntax-property left 'assign))
      (if mutate
        (mutate right)
        (plain-mutate left right)))))

;; Define an assignment operator that uses the current value in its operation
;;   a -= 2
;; is the same 
;;   a = a - 2
;; and turns into
;;   (set! a (- a 2))
;;
;;   a.x -= 2
;; turns into
;;   (honu-struct-set! a x (- (dot a x) 2))
;; where (dot a x) is field lookup of x inside a
(define-syntax-rule (define-honu-operator-= name operation)
  (define-honu-operator/syntax name 0.0001 'left
    (let ()
      (define (do-it left right)
        (with-syntax ([left left] [right right])
          #'(operation left right)))
      (define plain-mutate (mutator do-it))
      (lambda (left right)
        (define mutate (syntax-property left 'assign))
        (if mutate
          (mutate (do-it left right))
          (plain-mutate left right))))))

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
