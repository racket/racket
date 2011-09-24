#lang racket

;; Note: this queue doesn't implement the capacity restriction
;; of Mitchell and McKim's queue but this is easy to add.

;; a contract utility 
(define (all-but-last l) (reverse (cdr (reverse l))))
(define (eq/c x) (lambda (y) (eq? x y)))  

;; implementation
(define-struct queue (list p? eq))

(define (initialize p? eq) (make-queue '() p? eq))
(define items queue-list)
(define (put q x) 
  (make-queue (append (queue-list q) (list x))
              (queue-p? q)
              (queue-eq q)))
(define (count s) (length  (queue-list s)))
(define (is-empty? s) (null? (queue-list s)))
(define not-empty? (compose not is-empty?))
(define (rem s) 
  (make-queue (cdr (queue-list s)) 
              (queue-p? s) 
              (queue-eq s)))
(define (head s) (car (queue-list s)))

;; interface
(provide
 (contract-out
  ;; predicate 
  [queue?     (-> any/c boolean?)]
  
  ;; primitive queries
  ;; Imagine providing this 'query' for the interface of the module
  ;; only. Then in Racket there is no reason to have count or is-empty?
  ;; around (other than providing it to clients). After all items is 
  ;; exactly as cheap as count.
  [items      (->d ([q queue?]) () [result (listof (queue-p? q))])]
  
  ;; derived queries 
  [count      (->d ([q queue?]) 
                   ;; We could express this second part of the post
                   ;; condition even if count were a module "attribute"
                   ;; in the language of Eiffel; indeed it would use the 
                   ;; exact same syntax (minus the arrow and domain).
                   ()
                   [result (and/c natural-number/c
                                  (=/c (length (items q))))])]
  
  [is-empty?  (->d ([q queue?])
                   ()
                   [result (and/c boolean?
                                  (eq/c (null? (items q))))])]
  
  [head       (->d ([q (and/c queue? (compose not is-empty?))])
                   ()
                   [result (and/c (queue-p? q)
                                  (eq/c (car (items q))))])]
  ;; creation
  [initialize (-> contract? 
                  (contract? contract? . -> . boolean?) 
                  (and/c queue? (compose null? items)))]
  
  ;; commands
  [put        (->d ([oldq queue?] [i (queue-p? oldq)])
                   ()
                   [result
                    (and/c 
                     queue? 
                     (lambda (q)
                       (define old-items (items oldq))
                       (equal? (items q) (append old-items (list i)))))])]

  [rem        (->d ([oldq (and/c queue? (compose not is-empty?))])
                   ()
                   [result
                    (and/c queue?
                           (lambda (q)
                             (equal? (cdr (items oldq)) (items q))))])]))
;; end of interface


