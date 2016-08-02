#lang racket

(module reader racket
  (require syntax/strip-context)
 
  (provide (rename-out [literal-read read]
                       [literal-read-syntax read-syntax])
           get-info)
 
  (define (literal-read in)
    (syntax->datum
     (literal-read-syntax #f in)))
 
  (define (literal-read-syntax src in)
    (with-syntax ([str (port->string in)])
      (syntax-property
       (strip-context
        #'(module anything racket
            (require literal/show)
            (provide data)
            (define data 'str)
            (show data)))
       'module-language
       '#(literal/language-info get-language-info #f))))
 
  (define (get-info in mod line col pos)
    (lambda (key default)
      (case key
        [(color-lexer)
         (dynamic-require 'syntax-color/default-lexer
                          'default-lexer)]
        [else default]))))