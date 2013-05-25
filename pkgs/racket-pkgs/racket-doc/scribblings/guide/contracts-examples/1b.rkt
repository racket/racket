#lang racket

(require "1.rkt") ;; the module just above

;; implementation
;; [listof (list basic-customer? secret-info)]
(define all '())

(define (find c)
  (define (has-c-as-key p)
    (id-equal? (basic-customer-id (car p)) c))
  (define x (filter has-c-as-key all))
  (if (pair? x) (car x) x))

(define (active? c)
  (pair? (find c)))

(define not-active? (compose not active? basic-customer-id))

(define count 0)

(define (add c)
  (set! all (cons (list c 'secret) all))
  (set! count (+ count 1)))

(define (name id)
  (define bc-with-id (find id))
  (basic-customer-name (car bc-with-id)))

(define (set-name id name)
  (define bc-with-id (find id))
  (set-basic-customer-name! (car bc-with-id) name))

(define c0 0)
;; end of implementation

(provide
 (contract-out
  ;; how many customers are in the db?
  [count    natural-number/c]
  ;; is the customer with this id active?
  [active?  (-> id? boolean?)]
  ;; what is the name of the customer with this id?
  [name     (-> (and/c id? active?) string?)]
  ;; change the name of the customer with this id
  [set-name (->d ([id id?] [nn string?])
                 ()
                 [result any/c] ;; result contract
                 #:post-cond
                 (string=? (name id) nn))]

  [add      (->d ([bc (and/c basic-customer? not-active?)])
                 ()
                 ;; A pre-post condition contract must use
                 ;; a side-effect to express this contract
                 ;; via post-conditions
                 #:pre-cond (set! c0 count)
                 [result any/c] ;; result contract
                 #:post-cond (> count c0))]))
