#lang racket/base
(require racket/contract/base
         unstable/options)
(provide lexer/c)

(define (end/c start type)
  (cond
    [(equal? 'eof type) 
     (or/c exact-positive-integer? #f)]
    [start
     (and/c exact-positive-integer?
            (>/c start))]
    [else
     #f]))

(define lexer/c
  (option/c
   (or/c (->i ([in (and/c input-port? port-counts-lines?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]))
         (->i ([in (and/c input-port? port-counts-lines?)]
               [offset exact-nonnegative-integer?]
               [mode any/c])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]
                      [backup exact-nonnegative-integer?]
                      [new-mode any/c])))
   #:tester (λ (x) (and (procedure? x) 
                        (or (procedure-arity-includes? x 1)
                            (procedure-arity-includes? x 3))))))
