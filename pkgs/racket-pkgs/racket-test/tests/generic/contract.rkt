#lang racket/load

(module a racket/base
  (require racket/generic)

  (define-generics simple-dict
    (dict-ref    simple-dict key [default])
    (dict-set    simple-dict key val)
    (dict-remove simple-dict key))

  (provide dict-ref
           dict-set
           dict-remove
           gen:simple-dict
           simple-dict?
           simple-dict/c))

(module b racket/base
  (require 'a racket/contract)

  ;; same as in "alist.rkt"
  (define-struct alist (v)
    #:methods gen:simple-dict
    [(define (dict-ref dict key
                       [default (lambda () (error "key not found" key))])
       (cond [(assoc key (alist-v dict)) => cdr]
             [else (if (procedure? default) (default) default)]))
     (define (dict-set dict key val)
       (alist (cons (cons key val) (alist-v dict))))
     (define (dict-remove dict key)
       (define al (alist-v dict))
       (remove* (assoc key al) al))])

  (provide/contract
   [make-alist
    (-> list?
        (simple-dict/c
         [dict-ref (->* (simple-dict? symbol?) (any/c) integer?)]
         [dict-set (-> simple-dict? symbol? integer? simple-dict?)]
         [dict-remove (-> simple-dict? symbol? simple-dict?)]))]))

(module c racket/base
  (require 'a 'b rackunit)

  (define dict (make-alist '((a . 5) (b . 10))))
  (check-equal? (dict-ref dict 'a) 5)
  (check-equal? (dict-ref dict 'b) 10)
  (check-exn exn:fail:contract?
             (λ () (dict-set dict 'a "bad")))
  (check-exn exn:fail:contract?
             (λ () (dict-set dict "bad" 5))))

(require 'c)
