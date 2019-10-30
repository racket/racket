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
  (define d2
    '([a . 1]
      [a . 2]
      [a . 3]))

  (check-true (dict? d1))
  (check-eq? (dict-ref d1 1) 'a)
  (check-equal? (dict-count (dict-remove d1 2)) 1)
  (check-false (dict-mutable? d1))
  (check-true (dict-can-remove-keys? d1))
  (check-true (dict-can-functional-set? d1))

  (check-eq? (dict-ref d2 'a) 1)
  (check-equal? (dict-set d2 'a 4) '([a . 4] [a . 2] [a . 3]))
  (check-equal? (dict-remove d2 'a) '())
  (check-eq? (dict-count d2) 1)
  (check-equal? (dict-keys d2) '(a))
  (check-equal? (dict-values d2) '(1))
  (check-equal? (dict->list d2) '([a . 1]))
  (check-equal?
    (for/list ([{k v} (in-dict d2)])
      (cons k v))
    '([a . 1])))
