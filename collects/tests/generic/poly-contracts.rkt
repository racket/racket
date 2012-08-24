#lang racket

;; generics with parametric contracts

(require rackunit)

(module stack racket
  (require racket/generic)
  
  (define-generics stack
    (stack-push stack elem)
    (stack-pop stack)
    (stack-peek stack))
  
  (define (make-stack/c elem/c)
    (define rec-stack/c (recursive-contract (make-stack/c elem/c)))
    (stack/c
     [stack-push (-> rec-stack/c elem/c rec-stack/c)]
     [stack-pop (-> rec-stack/c rec-stack/c)]
     [stack-peek (-> rec-stack/c elem/c)]))

  (define elem/c (new-∀/c 'elem))

  (provide gen:stack
           stack?
           make-stack/c
           ;; generic functions have polymorphic contracts
           (contract-out 
            [stack-push (-> (make-stack/c elem/c) elem/c (make-stack/c elem/c))]
            [stack-pop (-> (make-stack/c elem/c) (make-stack/c elem/c))]
            [stack-peek (-> (make-stack/c elem/c) elem/c)])))
  
(module instance racket
  (require (submod ".." stack))
  
  (define-struct list-stack (l)
    #:methods gen:stack
    [(define (stack-push stack elem)
       (list-stack (cons elem (list-stack-l stack))))
     (define (stack-pop stack)
       (define lst (list-stack-l stack))
       (if (empty? lst)
           stack
           (list-stack (cdr lst))))
     (define (stack-peek stack)
       (car (list-stack-l stack)))])
  
  (provide
   (contract-out
    ;; specific instantiation of contract
    [list-stack (-> (listof symbol?) (make-stack/c symbol?))])))

(require 'stack 'instance)

(define stack (list-stack '(a b c)))

(check-true (stack? (stack-pop stack)))
(check-equal? (stack-peek stack) 'a)
(check-equal? (stack-peek (stack-push stack 'e)) 'e)
