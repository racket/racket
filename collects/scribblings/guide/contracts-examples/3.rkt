#lang racket

;; a shorthand for use below
(define-syntax ⇒ 
  (syntax-rules ()
    [(⇒ antecedent consequent) (if antecedent consequent #t)]))

;; implementation 
(define-struct dictionary (l value? eq?))
;; the keys should probably be another parameter (exercise)

(define (initialize p eq) (make-dictionary '() p eq))
(define (put d k v)
  (make-dictionary (cons (cons k v) (dictionary-l d))
                   (dictionary-value? d)
                   (dictionary-eq? d)))
(define (rem d k)
  (make-dictionary
   (let loop ([l (dictionary-l d)])
     (cond
       [(null? l) l]
       [(eq? (caar l) k) (loop (cdr l))]
       [else (cons (car l) (loop (cdr l)))]))
   (dictionary-value? d)
   (dictionary-eq? d)))
(define (count d) (length (dictionary-l d)))
(define (value-for d k) (cdr (assq k (dictionary-l d))))
(define (has? d k) (pair? (assq k (dictionary-l d))))
(define (not-has? d) (lambda (k) (not (has? d k))))
;; end of implementation 

;; interface
(provide
 (contract-out
  ;; predicates
  [dictionary? (-> any/c boolean?)]
  ;; basic queries
  ;; how many items are in the dictionary?
  [count       (-> dictionary? natural-number/c)]
  ;; does the dictionary define key k? 
  [has?        (->d ([d dictionary?] [k symbol?])
                    ()
                    [result boolean?]
                    #:post-cond
                    ((zero? (count d)) . ⇒ . (not result)))]
  ;; what is the value of key k in this dictionary? 
  [value-for   (->d ([d dictionary?]
                     [k (and/c symbol? (lambda (k) (has? d k)))])
                    ()
                    [result (dictionary-value? d)])]
  ;; initialization 
  ;; post condition: for all k in symbol, (has? d k) is false. 
  [initialize  (->d ([p contract?] [eq (p p . -> . boolean?)]) 
                    ()
                    [result (and/c dictionary? (compose zero? count))])]
  ;; commands 
  ;; Mitchell and McKim say that put shouldn't consume Void (null ptr) 
  ;; for v. We allow the client to specify a contract for all values
  ;; via initialize. We could do the same via a key? parameter
  ;; (exercise). add key k with value v to this dictionary 
  [put         (->d ([d dictionary?]
                     [k (and symbol? (not-has? d))]
                     [v (dictionary-value? d)])
                    ()
                    [result dictionary?]
                    #:post-cond
                    (and (has? result k) 
                         (= (count d) (- (count result) 1))
                         ([dictionary-eq? d] (value-for result k) v)))]
  ;; remove key k from this dictionary 
  [rem         (->d ([d dictionary?]
                     [k (and/c symbol? (lambda (k) (has? d k)))])
                    ()
                    [result (and/c dictionary? not-has?)]
                    #:post-cond
                    (= (count d) (+ (count result) 1)))]))
;; end of interface
