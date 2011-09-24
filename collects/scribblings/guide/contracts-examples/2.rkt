#lang racket

;; a contract utility 
(define (eq/c x) (lambda (y) (eq? x y)))

(define-struct stack (list p? eq))

(define (initialize p? eq) (make-stack '() p? eq))
(define (push s x) 
  (make-stack (cons x (stack-list s)) (stack-p? s) (stack-eq s)))
(define (item-at s i) (list-ref (reverse (stack-list s)) (- i 1)))
(define (count s) (length  (stack-list s)))
(define (is-empty? s) (null? (stack-list s)))
(define not-empty? (compose not is-empty?))
(define (pop s) (make-stack (cdr (stack-list s))
                            (stack-p? s)
                            (stack-eq s)))
(define (top s) (car (stack-list s)))

(provide
 (contract-out
  ;; predicate
  [stack?     (-> any/c boolean?)]
  
  ;; primitive queries
  ;; how many items are on the stack? 
  [count      (-> stack? natural-number/c)]
  
  ;; which item is at the given position? 
  [item-at
   (->d ([s stack?] [i (and/c positive? (<=/c (count s)))])
        ()
        [result (stack-p? s)])]
  
  ;; derived queries 
  ;; is the stack empty? 
  [is-empty?
   (->d ([s stack?])
        ()
        [result (eq/c (= (count s) 0))])]
  
  ;; which item is at the top of the stack 
  [top        
   (->d ([s (and/c stack? not-empty?)])
        ()
        [t (stack-p? s)] ;; a stack item, t is its name
        #:post-cond
        ([stack-eq s] t (item-at s (count s))))]
  
  ;; creation 
  [initialize
   (->d ([p contract?] [s (p p . -> . boolean?)])
        ()
        ;; Mitchell and McKim use (= (count s) 0) here to express
        ;; the post-condition in terms of a primitive query
        [result (and/c stack? is-empty?)])]
  
  ;; commands
  ;; add an item to the top of the stack 
  [push
   (->d ([s stack?] [x (stack-p? s)])
        ()
        [sn stack?] ;; result kind 
        #:post-cond
        (and (= (+ (count s) 1) (count sn))
             ([stack-eq s] x (top sn))))]
  
  ;; remove the item at the top of the stack
  [pop        
   (->d ([s (and/c stack? not-empty?)])
        ()
        [sn stack?] ;; result kind 
        #:post-cond
        (= (- (count s) 1) (count sn)))]))
