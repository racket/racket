#lang racket/base

(require racket/generic racket/dict racket/list)

(define-struct alist (v)
  #:methods gen:dict
  [(define (dict-ref dict key
                     [default (lambda () (error "key not found" key))])
     (cond [(assoc key (alist-v dict)) => cdr]
           [else (if (procedure? default) (default) default)]))
   (define (dict-set dict key val)
     (alist (cons (cons key val) (alist-v dict))))
   (define (dict-remove dict key)
     (define al (alist-v dict))
     (remove* (assoc key al) al))
   (define (dict-count dict)
     (length (remove-duplicates (alist-v dict) #:key car)))])


(module+ test
  (require rackunit)

  (define d1 '((1 . a) (2 . b)))

  (check-true (dict? d1))
  (check-eq? (dict-ref d1 1) 'a)
  (check-equal? (dict-count (dict-remove d1 2)) 1)
  (check-false (dict-mutable? d1))
  (check-true (dict-can-remove-keys? d1))
  (check-true (dict-can-functional-set? d1)))
