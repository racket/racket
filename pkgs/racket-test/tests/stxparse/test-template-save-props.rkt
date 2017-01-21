#lang racket
(require syntax/parse
         syntax/parse/experimental/template)

(begin-for-syntax
  (struct prefab-st (a b c) #:prefab)
  (struct st (a b c))
  (define (syntax-properties s . p*)
    (if (null? p*)
        s
        (apply syntax-properties
               (syntax-property s (car p*) (cadr p*))
               (cddr p*)))))

(define-syntax (define-with-prop stx)
  (syntax-case stx ()
    [(_ name)
     #`(define (name)
         (syntax-parse #'1
           [v
            (template #,(syntax-properties #'(v)
                                           'null '()
                                           'string "str"
                                           'bytes #"by"
                                           'number 123.4
                                           'boolean #t
                                           'char #\c
                                           'keyword '#:kw
                                           'regexp #rx".*"
                                           'pregexp #px".*"
                                           'byte-regexp #rx#".*"
                                           'byte-pregexp #px#".*"
                                           'box #&bx
                                           'symbol 'sym
                                           'pair '(a . b)
                                           'vector #(1 2 3)
                                           'hash #hash([a . 1] [b . 2])
                                           'hasheq #hasheq([a . 1] [b . 2])
                                           'hasheqv #hasheqv([a . 1] [b . 2])
                                           'prefab-st (prefab-st 'x 'y 'z)
                                           'st (st 'x 'y 'z))
                      #:properties (null
                                    string
                                    bytes
                                    number
                                    boolean
                                    char
                                    keyword
                                    regexp
                                    pregexp
                                    byte-regexp
                                    byte-pregexp
                                    box
                                    symbol
                                    pair
                                    vector
                                    hash
                                    hasheq
                                    hasheqv
                                    prefab-st
                                    st))]))]))

(define-with-prop get-syntax-with-saved-props)

(provide get-syntax-with-saved-props)