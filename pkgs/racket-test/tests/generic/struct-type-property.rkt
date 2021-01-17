#lang racket/base

(require racket/generic racket/match racket/dict rackunit)

(test-case "silly-dict"
  (define prop:silly-dict
    (make-generic-struct-type-property
     gen:dict
     (define (dict-ref self k [default #f])
       'i-am-a-dict)))
  (struct example ()
    #:transparent
    #:property prop:silly-dict #f)
  (check-equal? (dict-ref (example) 42) 'i-am-a-dict))

(test-case "container-repr"
  (define-generics repr (->sexpr repr)
    #:defaults
    ([number? (define (->sexpr n) n)]
     [symbol? (define (->sexpr s) `',s)]))
  (define-values [prop:container container? container-ref]
    (make-struct-type-property/generic
     'container
     #:methods gen:repr
     [(define/generic gen->sexpr ->sexpr)
      (define (->sexpr self)
        (match ((container-ref self) self)
          [(cons constructor contents)
           (cons constructor (map gen->sexpr contents))]))]))
  (struct foo (a b c)
    #:property prop:container
    (lambda (self)
      (list 'foo (foo-a self) (foo-b self) (foo-c self))))
  (check-equal? (->sexpr (foo 1 2 3)) '(foo 1 2 3))
  (check-equal? (->sexpr (foo 'a 'b 'c)) '(foo 'a 'b 'c))
  (check-equal? (->sexpr (foo 'a 1 (foo 'b 2 'empty)))
                '(foo 'a 1 (foo 'b 2 'empty))))

