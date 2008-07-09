#lang scheme/base

(provide (all-defined-out))

;; alist->eq : alist -> table
(define (alist->eq l) 
  (for/hasheq ([e l])
    (values (car e) (cdr e))))

(define (sexp->eq l) 
  (for/hasheq ([e l])
    (values (car e) (cadr e))))

;; to-sexp : table -> Listof(List k v)
(define (to-sexp t) (hash-map t list))

;; union/value : table(k,v) table(k,v) [(v v -> v)] -> table(k,v)
(define (union/value t1 t2 [f (lambda (x y) x)])
  (for/fold ([new-table t1])
	    ([(k v) t2])
	    (cond [(hash-ref new-table k #f)
		   => 
		   (lambda (v*) (hash-set new-table k (f v* v)))]
		  [else
		   (hash-set new-table k v)])))

(define (make-eq) (make-immutable-hasheq null))

(define (lookup k t) (hash-ref t k #f))

(define (insert k v t) (hash-set t k v))
